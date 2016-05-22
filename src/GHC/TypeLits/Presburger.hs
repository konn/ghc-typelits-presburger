{-# LANGUAGE MultiWayIf, PatternGuards, TupleSections #-}
module GHC.TypeLits.Presburger (plugin) where
import           Data.Foldable       (asum)
import           Data.Integer.SAT    (Expr (..), Prop (..), Prop, PropSet)
import           Data.Integer.SAT    (assert, checkSat, noProps, toName)
import qualified Data.Integer.SAT    as SAT
import           Data.List           (nub)
import           Data.Maybe          (fromMaybe, isNothing, mapMaybe)
import           GHC.TcPluginM.Extra (evByFiat)
import           GHC.TcPluginM.Extra (tracePlugin)
import           GhcPlugins          (EqRel (..), PredTree (..))
import           GhcPlugins          (classifyPredType, ppr)
import           GhcPlugins          (promotedTrueDataCon, tyConAppTyCon_maybe)
import           GhcPlugins          (typeKind, typeNatKind)
import           GhcPlugins          (mkTyConTy)
import           GhcPlugins          (promotedFalseDataCon)
import           Plugins             (Plugin (..), defaultPlugin)
import           TcPluginM           (TcPluginM, tcPluginTrace)
import           TcRnMonad           (Ct, TcPluginResult (..), isWanted)
import           TcRnTypes           (TcPlugin (..), ctEvPred, ctEvidence)
import           TcTypeNats          (typeNatAddTyCon, typeNatExpTyCon)
import           TcTypeNats          (typeNatLeqTyCon, typeNatMulTyCon)
import           TcTypeNats          (typeNatSubTyCon)
import           TypeRep             (TyLit (NumTyLit), Type (..))
import           Unique              (getKey, getUnique)

assert' :: Prop -> PropSet -> PropSet
assert' p ps = foldr assert ps (p : varPos)
  where
    varPos = [K 0 :<= Var i | i <- varsProp p ]

varsProp :: Prop -> [SAT.Name]
varsProp (p :|| q) = nub $ varsProp p ++ varsProp q
varsProp (p :&& q) = nub $ varsProp p ++ varsProp q
varsProp (Not p)   = varsProp p
varsProp (e :== v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :/= v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :< v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :> v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :<= v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :>= v) = nub $ varsExpr e ++ varsExpr v
varsProp _ = []

varsExpr :: Expr -> [SAT.Name]
varsExpr (e :+ v)   = nub $ varsExpr e ++ varsExpr v
varsExpr (e :- v)   = nub $ varsExpr e ++ varsExpr v
varsExpr (_ :* v)   = varsExpr v
varsExpr (Negate e) = varsExpr e
varsExpr (Var i)    = [i]
varsExpr (K _)      = []
varsExpr (If p e v) = nub $ varsProp p ++ varsExpr e ++ varsExpr v
varsExpr (Div e _)  = varsExpr e
varsExpr (Mod e _)  = varsExpr e

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just presburgerPlugin }

presburgerPlugin :: TcPlugin
presburgerPlugin = tracePlugin "typelits-presburger" $
                   TcPlugin { tcPluginInit  = return ()
                            , tcPluginSolve = decidePresburger
                            , tcPluginStop  = const $ return ()
                            }

testIf :: PropSet -> Prop -> Bool
testIf ps q = isNothing $ checkSat (Not q `assert'` ps)

decidePresburger :: () -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
decidePresburger () gs [] [] = do
  let givens = mapMaybe (\a -> (,) a <$> toPresburgerPred (deconsPred a)) gs
      prems  = foldr assert' noProps $ map snd givens
      (solved, _) = foldr go ([], noProps) givens
  if isNothing (checkSat prems)
    then return $ TcPluginContradiction gs
    else return $ TcPluginOk (map (undefined,) solved) []
  where
    go (ct, p) (ss, prem)
      | testIf prem p = (ct : ss, prem)
      | otherwise = (ss, assert' p prem)

decidePresburger () gs ds ws = do
  tcPluginTrace "wanteds" (ppr ws)
  let wants = mapMaybe (\ct -> (,) ct <$> toPresburgerPred (deconsPred ct)) $
              filter (isWanted . ctEvidence) ws
      prems = foldr assert' noProps $
              mapMaybe (toPresburgerPred . deconsPred) $ filter (isWanted . ctEvidence) (ds ++ gs)
      solved = map fst $ filter (testIf prems . snd) wants
      coerced = [(evByFiat "ghc-typelits-presburger" t1 t2, ct)
                | ct <- solved
                , EqPred NomEq t1 t2 <- return (deconsPred ct)
                ]
      leq'd = [] -- [(mkLeqEv t1 t2, ct)
              -- | ct <- solved
              -- , ClassPred cls [t1, t2] <- return (deconsPred ct)]
  if isNothing $ checkSat (foldr (assert' . snd) noProps wants)
    then return $ TcPluginContradiction $ map fst wants
    else return $ TcPluginOk (coerced ++ leq'd) []

deconsPred :: Ct -> PredTree
deconsPred = classifyPredType . ctEvPred . ctEvidence

toPresburgerPred :: PredTree -> Maybe Prop
toPresburgerPred (EqPred NomEq p false) -- P ~ 'False <=> Not P ~ 'True
  | Just promotedFalseDataCon  == tyConAppTyCon_maybe false =
    Not <$> toPresburgerPred (EqPred NomEq p (mkTyConTy promotedTrueDataCon))
toPresburgerPred (EqPred NomEq p b)  -- (n :<=? m) ~ 'True
  | Just promotedTrueDataCon  == tyConAppTyCon_maybe b
  , TyConApp con [t1, t2] <- p
  , con == typeNatLeqTyCon = (:<=) <$> toPresburgerExp t1  <*> toPresburgerExp t2
toPresburgerPred (EqPred NomEq t1 t2) -- (n :: Nat) ~ (m :: Nat)
  | typeKind t1 == typeNatKind = (:==) <$> toPresburgerExp t1 <*> toPresburgerExp t2
toPresburgerPred _ = Nothing

toPresburgerExp :: Type -> Maybe Expr
toPresburgerExp (TyVarTy t) = Just $ Var $ toName $ getKey $ getUnique t
toPresburgerExp (TyConApp tc ts)
  | tc == typeNatMulTyCon, [tl, tr] <- ts =
    case (simpleExp tl, simpleExp tr) of
      (LitTy (NumTyLit n), LitTy (NumTyLit m)) -> Just $ K $ n * m
      (LitTy (NumTyLit n), x) -> (:*) <$> pure n <*> toPresburgerExp x
      (x, LitTy (NumTyLit n)) -> (:*) <$> pure n <*> toPresburgerExp x
      _ -> Nothing
  | otherwise =  asum [ step con op
                      | (con, op) <- [(typeNatAddTyCon, (:+)), (typeNatSubTyCon, (:-))]]
  where
    step con op
      | tc == con, [tl, tr] <- ts =
        op <$> toPresburgerExp tl <*> toPresburgerExp tr
      | otherwise = Nothing
toPresburgerExp (LitTy (NumTyLit n)) = Just (K n)
toPresburgerExp _ = Nothing

simpleExp :: Type -> Type
simpleExp (TyVarTy t) = TyVarTy t
simpleExp (AppTy t1 t2) = AppTy (simpleExp t1) (simpleExp t2)
simpleExp (FunTy t1 t2) = FunTy (simpleExp t1) (simpleExp t2)
simpleExp (ForAllTy t1 t2) = ForAllTy t1 (simpleExp t2)
simpleExp (LitTy t) = LitTy t
simpleExp (TyConApp tc ts) = fromMaybe (TyConApp tc (map simpleExp ts)) $
  asum (map simpler [(typeNatAddTyCon, (+))
                    ,(typeNatSubTyCon, (-))
                    ,(typeNatMulTyCon, (*))
                    ,(typeNatExpTyCon, (^))
                    ])
  where
    simpler (con, op)
      | con == tc, [tl, tr] <- map simpleExp ts =
        Just $
        case (tl, tr) of
          (LitTy (NumTyLit n), LitTy (NumTyLit m)) -> LitTy (NumTyLit (op n m))
          _ -> TyConApp con [tl, tr]
      | otherwise = Nothing

