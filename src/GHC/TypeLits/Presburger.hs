{-# LANGUAGE MultiWayIf, PatternGuards, TupleSections, ViewPatterns #-}
module GHC.TypeLits.Presburger (plugin) where
import GHC.Compat

import           Class               (classTyCon)
import           Data.Foldable       (asum)
import           Data.Integer.SAT    (Expr (..), Prop (..), Prop, PropSet)
import           Data.Integer.SAT    (assert, checkSat, noProps, toName)
import qualified Data.Integer.SAT    as SAT
import           Data.List           (nub)
import           Data.Maybe          (fromMaybe, isNothing, mapMaybe)
import           GHC.TcPluginM.Extra (evByFiat)
import           GHC.TcPluginM.Extra (tracePlugin)

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
presburgerPlugin =
  tracePlugin "typelits-presburger" $
  TcPlugin { tcPluginInit  = return () -- tcPluginIO $ newIORef emptyTvSubst
           , tcPluginSolve = decidePresburger
           , tcPluginStop  = const $ return ()
           }

testIf :: PropSet -> Prop -> Bool
testIf ps q = isNothing $ checkSat (Not q `assert'` ps)

type PresState = ()

decidePresburger :: PresState -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
decidePresburger _ref gs [] [] = do
  tcPluginTrace "Started givens with: " (ppr gs)
  let subst = emptyTvSubst
              -- foldr unionTvSubst emptyTvSubst $ map genSubst gs
      givens = mapMaybe (\a -> (,) a <$> toPresburgerPred subst (deconsPred a)) gs
      prems0 = map snd givens
      prems  = foldr assert' noProps prems0
      (solved, _) = foldr go ([], noProps) givens
  if isNothing (checkSat prems)
    then return $ TcPluginContradiction gs
    else return $ TcPluginOk (map withEv solved) []
  where
    go (ct, p) (ss, prem)
      | testIf prem p = (ct : ss, prem)
      | otherwise = (ss, assert' p prem)
decidePresburger _ref gs ds ws = do
  let subst = foldr unionTvSubst emptyTvSubst $ map genSubst (gs ++ ds)
  tcPluginTrace "Current subst" (ppr subst)
  tcPluginTrace "wanteds" (ppr ws)
  tcPluginTrace "givens" (ppr gs)
  tcPluginTrace "driveds" (ppr ds)
  let wants = mapMaybe (\ct -> (,) ct <$> toPresburgerPred subst (deconsPred ct)) $
              filter (isWanted . ctEvidence) ws
      prems = foldr assert' noProps $
              mapMaybe (toPresburgerPred subst . deconsPred) (gs ++ ds)
      solved = map fst $ filter (testIf prems . snd) wants
      coerced = [(evByFiat "ghc-typelits-presburger" t1 t2, ct)
                | ct <- solved
                , EqPred NomEq t1 t2 <- return (classifyPredType $ deconsPred ct)
                ]
  tcPluginTrace "prems" (text $ show prems)
  tcPluginTrace "final goals" (text $ show $ map snd wants)
  if isNothing $ checkSat (foldr (assert' . snd) noProps wants)
    then return $ TcPluginContradiction $ map fst wants
    else return $ TcPluginOk coerced []

(<=>) :: Prop -> Prop -> Prop
p <=> q =  (p :&& q) :|| (Not p :&& Not q)

genSubst :: Ct -> TvSubst
genSubst ct = case classifyPredType (deconsPred ct) of
  EqPred NomEq t u -> fromMaybe emptyTvSubst $ tcUnifyTy t u
  _ -> emptyTvSubst

withEv :: Ct -> (EvTerm, Ct)
withEv ct
  | EqPred _ t1 t2 <- classifyPredType (deconsPred ct) =
      (evByFiat "ghc-typelits-presburger" t1 t2, ct)
  | otherwise = undefined

deconsPred :: Ct -> Type
deconsPred = ctEvPred . ctEvidence

toPresburgerPred :: TvSubst -> Type -> Maybe Prop
toPresburgerPred subst (TyConApp con [t1, t2])
  | con == typeNatLeqTyCon = (:<=) <$> toPresburgerExp subst t1 <*> toPresburgerExp subst t2
toPresburgerPred subst ty
  | isEqPred ty = toPresburgerPredTree subst $ classifyPredType ty
  | otherwise = Nothing

toPresburgerPredTree :: TvSubst -> PredTree -> Maybe Prop
toPresburgerPredTree subst (EqPred NomEq p false) -- P ~ 'False <=> Not P ~ 'True
  | Just promotedFalseDataCon  == tyConAppTyCon_maybe (substTy subst false) =
    Not <$> toPresburgerPredTree subst (EqPred NomEq p (mkTyConTy promotedTrueDataCon))
toPresburgerPredTree subst (EqPred NomEq p b)  -- (n :<=? m) ~ 'True
  | Just promotedTrueDataCon  == tyConAppTyCon_maybe (substTy subst b)
  , TyConApp con [t1, t2] <- substTy subst p
  , con == typeNatLeqTyCon = (:<=) <$> toPresburgerExp subst t1  <*> toPresburgerExp subst t2
toPresburgerPredTree subst (EqPred NomEq p q)  -- (p :: Bool) ~ (q :: Bool)
  | typeKind p `eqType` mkTyConTy promotedBoolTyCon =
    (<=>) <$> toPresburgerPred subst p
          <*> toPresburgerPred subst q
toPresburgerPredTree subst (EqPred NomEq t1 t2) -- (n :: Nat) ~ (m :: Nat)
  | typeKind t1 `eqType` typeNatKind = (:==) <$> toPresburgerExp subst t1 <*> toPresburgerExp subst t2
toPresburgerPredTree subst (ClassPred con [t1, t2]) -- (n :: Nat) ~ (m :: Nat)
  | typeNatLeqTyCon == classTyCon con
  , typeKind t1 `eqType` typeNatKind = (:<=) <$> toPresburgerExp subst t1 <*> toPresburgerExp subst t2
toPresburgerPredTree _ _ = Nothing

toPresburgerExp :: TvSubst -> Type -> Maybe Expr
toPresburgerExp dic ty = case substTy dic ty of
  TyVarTy t -> Just $ Var $ toName $ getKey $ getUnique t
  TyConApp tc ts  ->
    let step con op
          | tc == con, [tl, tr] <- ts =
            op <$> toPresburgerExp dic tl <*> toPresburgerExp dic tr
          | otherwise = Nothing
    in case ts of
      [tl, tr] | tc == typeNatMulTyCon ->
        case (simpleExp tl, simpleExp tr) of
          (LitTy (NumTyLit n), LitTy (NumTyLit m)) -> Just $ K $ n * m
          (LitTy (NumTyLit n), x) -> (:*) <$> pure n <*> toPresburgerExp dic x
          (x, LitTy (NumTyLit n)) -> (:*) <$> pure n <*> toPresburgerExp dic x
          _ -> Nothing
      _ ->  asum [ step con op
                 | (con, op) <- [(typeNatAddTyCon, (:+)), (typeNatSubTyCon, (:-))]]
  LitTy (NumTyLit n) -> Just (K n)
  _ -> Nothing

simpleExp :: Type -> Type
simpleExp (AppTy t1 t2) = AppTy (simpleExp t1) (simpleExp t2)
simpleExp (FunTy t1 t2) = FunTy (simpleExp t1) (simpleExp t2)
simpleExp (ForAllTy t1 t2) = ForAllTy t1 (simpleExp t2)
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
simpleExp t = t

