{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings, PatternGuards #-}
{-# LANGUAGE RankNTypes, RecordWildCards, TupleSections                     #-}
module GHC.TypeLits.Presburger (plugin) where
import GHC.Compat

import           Class            (classTyCon)
import           Data.Foldable    (asum)
import           Data.Integer.SAT (Expr (..), Prop (..), PropSet, assert)
import           Data.Integer.SAT (checkSat, noProps, toName)
import qualified Data.Integer.SAT as SAT
import           Data.List        (nub)
import           Data.Maybe       (fromMaybe, isNothing, mapMaybe)
import           Data.Reflection  (Given, give, given)
import           TcPluginM        (lookupOrig, tcLookupClass)
import           TysWiredIn       (promotedEQDataCon, promotedGTDataCon,
                                   promotedLTDataCon)

assert' :: Prop -> PropSet -> PropSet
assert' p ps = foldr assert ps (p : varPos)
  where
    varPos = [K 0 :<= Var i | i <- varsProp p ]

data Proof = Proved | Disproved [(Int, Integer)]
           deriving (Read, Show, Eq, Ord)

isProved :: Proof -> Bool
isProved Proved = True
isProved _      = False

varsProp :: Prop -> [SAT.Name]
varsProp (p :|| q) = nub $ varsProp p ++ varsProp q
varsProp (p :&& q) = nub $ varsProp p ++ varsProp q
varsProp (Not p)   = varsProp p
varsProp (e :== v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :/= v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :< v)  = nub $ varsExpr e ++ varsExpr v
varsProp (e :> v)  = nub $ varsExpr e ++ varsExpr v
varsProp (e :<= v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :>= v) = nub $ varsExpr e ++ varsExpr v
varsProp _         = []

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
  tracePlugin "typelits-presburger"
  TcPlugin { tcPluginInit  = return () -- tcPluginIO $ newIORef emptyTvSubst
           , tcPluginSolve = decidePresburger
           , tcPluginStop  = const $ return ()
           }

testIf :: PropSet -> Prop -> Proof
testIf ps q = maybe Proved Disproved $ checkSat (Not q `assert'` ps)

type PresState = ()

data MyEnv  = MyEnv { emptyClsTyCon     :: TyCon
                    , eqTyCon_          :: TyCon
                    , eqWitCon_         :: TyCon
                    , isTrueCon_        :: TyCon
                    , voidTyCon         :: TyCon
                    , typeLeqBoolTyCon_ :: TyCon
                    , singCompareCon_   :: TyCon
                    }

eqTyCon :: Given MyEnv => TyCon
eqTyCon = eqTyCon_ given

eqWitnessTyCon :: Given MyEnv => TyCon
eqWitnessTyCon = eqWitCon_ given

isTrueTyCon :: Given MyEnv => TyCon
isTrueTyCon = isTrueCon_ given

typeLeqBoolTyCon :: Given MyEnv => TyCon
typeLeqBoolTyCon = typeLeqBoolTyCon_ given

singCompareCon :: Given MyEnv => TyCon
singCompareCon = singCompareCon_ given

decidePresburger :: PresState -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
decidePresburger _ref gs [] [] = do
  tcPluginTrace "Started givens with: " (ppr $ map (ctEvPred . ctEvidence) gs)
  withTyCons $ do
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
        | Proved <- testIf prem p = (ct : ss, prem)
        | otherwise = (ss, assert' p prem)
decidePresburger _ref gs ds ws = withTyCons $ do
  tcPluginTrace "Env" $ ppr (emptyTyCon, eqTyCon, eqWitnessTyCon, isTrueTyCon)
  let subst = foldr (unionTvSubst . genSubst) emptyTvSubst (gs ++ ds)
  tcPluginTrace "Current subst" (ppr subst)
  tcPluginTrace "wanteds" $ ppr $ map deconsPred ws
  tcPluginTrace "givens" $ ppr $ map (substTy subst . deconsPred) gs
  tcPluginTrace "deriveds" $ ppr $ map deconsPred ds
  let wants = mapMaybe (\ct -> (,) ct <$> toPresburgerPred subst (substTy subst $ deconsPred ct)) $
              filter (isWanted . ctEvidence) ws
      prems = foldr assert' noProps $
              mapMaybe (toPresburgerPred subst . substTy subst . deconsPred) (gs ++ ds)
      solved = map fst $ filter (isProved . testIf prems . snd) wants
      coerced = [(evByFiat "ghc-typelits-presburger" t1 t2, ct)
                | ct <- solved
                , EqPred NomEq t1 t2 <- return (classifyPredType $ deconsPred ct)
                ]
  tcPluginTrace "prems" (text $ show $ map (toPresburgerPred subst .substTy subst . deconsPred) (gs ++ ds))
  tcPluginTrace "final goals" (text $ show $ map snd wants)
  case testIf prems (foldr ((:&&) . snd) PTrue wants) of
    Proved -> do
      tcPluginTrace "Proved" (text $ show $ map snd wants)
      return $ TcPluginOk coerced []
    Disproved wit -> do
      tcPluginTrace "Failed! " (text $ show wit)
      return $ TcPluginContradiction $ map fst wants

withTyCons :: (Given MyEnv => TcPluginM a) -> TcPluginM a
withTyCons act = do
  emd <- lookupModule (mkModuleName "Proof.Propositional.Empty") (fsLit "equational-reasoning")
  emptyClsTyCon <- classTyCon <$> (tcLookupClass =<< lookupOrig emd (mkTcOcc "Empty"))
  eqTyCon_ <- getEqTyCon
  eqWitCon_ <- getEqWitnessTyCon
  pmd <- lookupModule (mkModuleName "Proof.Propositional") (fsLit "equational-reasoning")
  isTrueCon_ <- tcLookupTyCon =<< lookupOrig pmd (mkTcOcc "IsTrue")
  vmd <- lookupModule (mkModuleName "Data.Void") (fsLit "base")
  voidTyCon <- tcLookupTyCon =<< lookupOrig vmd (mkTcOcc "Void")
  singletons <- lookupModule (mkModuleName "Data.Singletons.Prelude.Ord") (fsLit "singletons")
  typeLeqBoolTyCon_ <- tcLookupTyCon =<< lookupOrig singletons (mkTcOcc "<=")
  singCompareCon_ <- tcLookupTyCon =<< lookupOrig singletons (mkTcOcc "Compare")
  give MyEnv{..} act

(<=>) :: Prop -> Prop -> Prop
p <=> q =  (p :&& q) :|| (Not p :&& Not q)

genSubst :: Ct -> TvSubst
genSubst ct = case classifyPredType (deconsPred ct) of
  EqPred NomEq t u -> fromMaybe emptyTvSubst $ tcUnifyTy t u
  _                -> emptyTvSubst

withEv :: Ct -> (EvTerm, Ct)
withEv ct
  | EqPred _ t1 t2 <- classifyPredType (deconsPred ct) =
      (evByFiat "ghc-typelits-presburger" t1 t2, ct)
  | otherwise = undefined

deconsPred :: Ct -> Type
deconsPred = ctEvPred . ctEvidence

emptyTyCon :: Given MyEnv => TyCon
emptyTyCon = emptyClsTyCon given

toPresburgerPred :: Given MyEnv => TvSubst -> Type -> Maybe Prop
toPresburgerPred subst (TyConApp con [t1, t2])
  | con == typeNatLeqTyCon = (:<=) <$> toPresburgerExp subst t1 <*> toPresburgerExp subst t2
toPresburgerPred subst ty
  | isEqPred ty = toPresburgerPredTree subst $ classifyPredType ty
  | Just (con, [l, r]) <- splitTyConApp_maybe ty -- l ~ r
  , con == eqTyCon = toPresburgerPredTree subst $ EqPred NomEq l r
  | Just (con, [_k, l, r]) <- splitTyConApp_maybe ty -- l (:~: {k}) r
  , con == eqWitnessTyCon = toPresburgerPredTree subst $ EqPred NomEq l r
  | Just (con, [l]) <- splitTyConApp_maybe ty -- Empty l => ...
  , con == emptyTyCon = Not <$> toPresburgerPred subst l
  | Just (con, [l]) <- splitTyConApp_maybe ty -- IsTrue l =>
  , con == isTrueTyCon = toPresburgerPred subst l
  | otherwise = Nothing

boolLeqs :: Given MyEnv => [TyCon]
boolLeqs = [typeNatLeqTyCon, typeLeqBoolTyCon]

toPresburgerPredTree :: Given MyEnv => TvSubst -> PredTree -> Maybe Prop
toPresburgerPredTree subst (EqPred NomEq p false) -- P ~ 'False <=> Not P ~ 'True
  | Just promotedFalseDataCon  == tyConAppTyCon_maybe (substTy subst false) =
    Not <$> toPresburgerPredTree subst (EqPred NomEq p (mkTyConTy promotedTrueDataCon))
toPresburgerPredTree subst (EqPred NomEq p b)  -- (n :<=? m) ~ 'True
  | Just promotedTrueDataCon  == tyConAppTyCon_maybe (substTy subst b)
  , Just (con, [t1, t2]) <- splitTyConApp_maybe (substTy subst p)
  , con `elem` boolLeqs = (:<=) <$> toPresburgerExp subst t1  <*> toPresburgerExp subst t2
toPresburgerPredTree subst (EqPred NomEq p q)  -- (p :: Bool) ~ (q :: Bool)
  | typeKind p `eqType` mkTyConTy promotedBoolTyCon =
    (<=>) <$> toPresburgerPred subst p
          <*> toPresburgerPred subst q
toPresburgerPredTree subst (EqPred NomEq n m)  -- (n :: Nat) ~ (m :: Nat)
  | typeKind n `eqType` typeNatKind =
    (:==) <$> toPresburgerExp subst n
          <*> toPresburgerExp subst m
toPresburgerPredTree subst (EqPred _ t1 t2) -- CmpNat a b ~ CmpNat c d
  | Just (con,  [a, b]) <- splitTyConApp_maybe (substTy subst t1)
  , Just (con', [c, d]) <- splitTyConApp_maybe (substTy subst t2)
  , con `elem` [singCompareCon, typeNatCmpTyCon], con' `elem` [typeNatCmpTyCon, singCompareCon]
  = (<=>) <$> ((:<) <$> toPresburgerExp subst a <*> toPresburgerExp subst b)
          <*> ((:<) <$> toPresburgerExp subst c <*> toPresburgerExp subst d)
toPresburgerPredTree subst (EqPred NomEq t1 t2) -- CmpNat a b ~ x
  | Just (con, [a, b]) <- splitTyConApp_maybe (substTy subst t1)
  , con `elem` [typeNatCmpTyCon, singCompareCon]
  , Just cmp <- tyConAppTyCon_maybe (substTy subst t2) =
    let dic = [(promotedLTDataCon, (:<))
              ,(promotedEQDataCon, (:==))
              ,(promotedGTDataCon, (:>))
              ]
    in lookup cmp dic <*> toPresburgerExp subst a
                      <*> toPresburgerExp subst b
toPresburgerPredTree subst (EqPred NomEq t1 t2) -- x ~ CmpNat a b
  | Just (con, [a, b]) <- splitTyConApp_maybe (substTy subst t2)
  , con `elem` [singCompareCon, typeNatCmpTyCon]
  , Just cmp <- tyConAppTyCon_maybe (substTy subst t1) =
    let dic = [(promotedLTDataCon, (:<))
              ,(promotedEQDataCon, (:==))
              ,(promotedGTDataCon, (:>))
              ]
    in lookup cmp dic <*> toPresburgerExp subst a
                      <*> toPresburgerExp subst b
toPresburgerPredTree subst (ClassPred con [t1, t2]) -- (n :: Nat) <= (m :: Nat)
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

-- simplTypeCmp :: Type -> Type

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

