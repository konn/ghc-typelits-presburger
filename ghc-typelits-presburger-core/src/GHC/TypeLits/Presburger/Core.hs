{-# LANGUAGE CPP, DataKinds, FlexibleContexts, FlexibleInstances      #-}
{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, PatternGuards #-}
{-# LANGUAGE RankNTypes, RecordWildCards, TypeOperators               #-}
{-# LANGUAGE TypeSynonymInstances                                     #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module GHC.TypeLits.Presburger.Core (plugin) where
import GHC.Compat

import           Class                          (Class, classTyCon)
import           Control.Applicative            ((<|>))
import           Control.Arrow                  (second)
import           Control.Monad                  (forM_, mzero, unless)
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Control.Monad.Trans.RWS.Strict (runRWS, tell)
import           Control.Monad.Trans.State      (StateT, runStateT)
import           Data.Foldable                  (asum)
import           Data.Integer.SAT               (Expr (..), Prop (..), PropSet,
                                                 assert)
import           Data.Integer.SAT               (checkSat, noProps, toName)
import qualified Data.Integer.SAT               as SAT
import qualified Data.IntSet                    as IS
import           Data.List                      (nub)
import qualified Data.Map.Strict                as M
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (catMaybes, fromJust)
import           Data.Maybe                     (fromMaybe, isNothing, mapMaybe)
import           Data.Reflection                (Given, give, given)
import           Data.Semigroup                 (Max (..), Option (..))
import qualified Data.Set                       as Set
import           GHC.TypeLits                   (Nat)
import           Outputable                     (showSDocUnsafe)
import           TcPluginM                      (getFamInstEnvs, lookupOrig,
                                                 matchFam, newFlexiTyVar,
                                                 newWanted, tcLookupClass,
                                                 unsafeTcPluginTcM)
import           TcRnTypes
import           TyCoRep                        (Coercion (..),
                                                 KindCoercion (..))
import           Type                           (mkPrimEqPredRole, mkTyVarTy,
                                                 splitTyConApp)
import           TysWiredIn                     (promotedEQDataCon,
                                                 promotedGTDataCon,
                                                 promotedLTDataCon)
import           Var

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

-- Replaces every subtraction with new constant,
-- adding order constraint.
handleSubtraction :: Prop -> Prop
handleSubtraction p0 =
  let (p, _, w) = runRWS (loop p0) () Set.empty
  in foldr (:&&) p w
  where
    loop PTrue     = return PTrue
    loop PFalse    = return PFalse
    loop (q :|| r) = (:||) <$> loop q <*> loop r
    loop (q :&& r) = (:&&) <$> loop q <*> loop r
    loop (Not q)   = Not <$> loop q
    loop (l :<= r) = (:<=) <$> loopExp l <*> loopExp r
    loop (l :< r)  = (:<) <$> loopExp l <*> loopExp r
    loop (l :>= r) = (:<=) <$> loopExp l <*> loopExp r
    loop (l :> r)  = (:<) <$> loopExp l <*> loopExp r
    loop (l :== r) = (:==) <$> loopExp l <*> loopExp r
    loop (l :/= r) = (:/=) <$> loopExp l <*> loopExp r


    withPositive pos = do
      dic <- get
      unless (Set.member pos dic) $ do
        modify $ Set.insert pos
        tell $ Set.fromList [pos :>= K 0]
      return pos

    loopExp e@(Negate _) = withPositive . Negate =<< loopExp e
    loopExp (l :- r)   = do
      e <- (:-) <$> loopExp l <*> loopExp r
      withPositive e
    loopExp (l :+ r)     = (:+) <$> loopExp l <*> loopExp r
    loopExp v@Var {}     = return v
    loopExp (c :* e)
      | c > 0 = (c :*) <$> loopExp e
      | otherwise = (negate c :*) <$> loopExp (Negate e)
    loopExp e@(K _) = return e


type PresState = ()

data MyEnv  = MyEnv { emptyClsTyCon :: TyCon
                    , eqTyCon_      :: TyCon
                    , eqWitCon_     :: TyCon
                    , isTrueCon_    :: TyCon
                    , voidTyCon     :: TyCon
                    }

eqTyCon :: Given MyEnv => TyCon
eqTyCon = eqTyCon_ given

eqWitnessTyCon :: Given MyEnv => TyCon
eqWitnessTyCon = eqWitCon_ given

isTrueTyCon :: Given MyEnv => TyCon
isTrueTyCon = isTrueCon_ given

decidePresburger :: PresState -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
decidePresburger _ref gs [] [] = do
  tcPluginTrace "Started givens with: " (ppr $ map (ctEvPred . ctEvidence) gs)
  withTyCons $ do
    let subst = emptyTvSubst
                -- foldr unionTvSubst emptyTvSubst $ map genSubst gs
    ngs <- mapM (\a -> runMachine $ (,) a <$> toPresburgerPred subst (deconsPred a)) gs
    let givens = catMaybes ngs
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
  (prems, wants, prems0) <- do
    wants <- catMaybes <$>
             mapM
             (\ct -> runMachine $ (,) ct <$> toPresburgerPred subst (substTy subst $ deconsPred ct))
             (filter (isWanted . ctEvidence) ws)
    resls <- mapM (runMachine . toPresburgerPred subst . substTy subst . deconsPred)
                     (gs ++ ds)
    let prems = foldr assert' noProps $ catMaybes resls
    return (prems, map (second handleSubtraction) wants, catMaybes resls)
  let solved = map fst $ filter (isProved . testIf prems . snd) wants
      coerced = [(evByFiat "ghc-typelits-presburger" t1 t2, ct)
                | ct <- solved
                , EqPred NomEq t1 t2 <- return (classifyPredType $ deconsPred ct)
                ]
  tcPluginTrace "final premises" (text $ show prems0)
  tcPluginTrace "final goals" (text $ show $ map snd wants)
  case testIf prems (foldr ((:&&) . snd) PTrue wants) of
    Proved -> do
      tcPluginTrace "Proved" (text $ show $ map snd wants)
      tcPluginTrace "... with coercions" (ppr coerced)
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

toPresburgerPred :: Given MyEnv => TvSubst -> Type -> Machine Prop
toPresburgerPred subst (TyConApp con [t1, t2])
  | con == typeNatLeqTyCon = (:<=) <$> toPresburgerExp subst t1 <*> toPresburgerExp subst t2
toPresburgerPred subst ty
  | Just (con, []) <- splitTyConApp_maybe ty
  , con == promotedTrueDataCon = return PTrue
  | Just (con, []) <- splitTyConApp_maybe ty
  , con == promotedFalseDataCon = return PFalse
  | isEqPred ty = toPresburgerPredTree subst $ classifyPredType ty
  | Just (con, [l, r]) <- splitTyConApp_maybe ty -- l ~ r
  , con == eqTyCon = toPresburgerPredTree subst $ EqPred NomEq l r
  | Just (con, [_k, l, r]) <- splitTyConApp_maybe ty -- l (:~: {k}) r
  , con == eqWitnessTyCon = toPresburgerPredTree subst $ EqPred NomEq l r
  | Just (con, [l]) <- splitTyConApp_maybe ty -- Empty l => ...
  , con == emptyTyCon = Not <$> toPresburgerPred subst l
  | Just (con, [l]) <- splitTyConApp_maybe ty -- IsTrue l =>
  , con == isTrueTyCon = toPresburgerPred subst l
  | otherwise = mzero

boolLeqs :: Given MyEnv => [TyCon]
boolLeqs = [typeNatLeqTyCon]

toPresburgerPredTree :: Given MyEnv => TvSubst -> PredTree -> Machine Prop
toPresburgerPredTree subst (EqPred NomEq p false) -- P ~ 'False <=> Not P ~ 'True
  | Just promotedFalseDataCon  == tyConAppTyCon_maybe (substTy subst false) =
    Not <$> toPresburgerPredTree subst (EqPred NomEq p (mkTyConTy promotedTrueDataCon))
toPresburgerPredTree subst (EqPred NomEq p b)  -- (n :<=? m) ~ 'True
  | Just promotedTrueDataCon  == tyConAppTyCon_maybe (substTy subst b)
  , Just (con, [t1, t2]) <- splitTyConApp_maybe (substTy subst p)
  , con `elem` boolLeqs = (:<=) <$> toPresburgerExp subst t1  <*> toPresburgerExp subst t2
toPresburgerPredTree subst (EqPred NomEq p q)  -- (p :: Bool) ~ (q :: Bool)
    | typeKind p `eqType` mkTyConTy promotedBoolTyCon = do
      lift $ lift $ tcPluginTrace "EQBOOL:" $ ppr (p, q)
      (<=>) <$> toPresburgerPred subst p
            <*> toPresburgerPred subst q
toPresburgerPredTree subst (EqPred NomEq n m)  -- (n :: Nat) ~ (m :: Nat)
  | typeKind n `eqType` typeNatKind =
    (:==) <$> toPresburgerExp subst n
          <*> toPresburgerExp subst m
toPresburgerPredTree subst (EqPred _ t1 t2) -- CmpNat a b ~ CmpNat c d
  | Just (con,  [a, b]) <- splitTyConApp_maybe (substTy subst t1)
  , Just (con', [c, d]) <- splitTyConApp_maybe (substTy subst t2)
  , con `elem` [typeNatCmpTyCon], con' `elem` [typeNatCmpTyCon]
  = (<=>) <$> ((:<) <$> toPresburgerExp subst a <*> toPresburgerExp subst b)
          <*> ((:<) <$> toPresburgerExp subst c <*> toPresburgerExp subst d)
toPresburgerPredTree subst (EqPred NomEq t1 t2) -- CmpNat a b ~ x
  | Just (con, [a, b]) <- splitTyConApp_maybe (substTy subst t1)
  , con `elem` [typeNatCmpTyCon]
  , Just cmp <- tyConAppTyCon_maybe (substTy subst t2) =
    let dic = [(promotedLTDataCon, (:<))
              ,(promotedEQDataCon, (:==))
              ,(promotedGTDataCon, (:>))
              ]
    in MaybeT (return $ lookup cmp dic)
       <*> toPresburgerExp subst a
       <*> toPresburgerExp subst b
toPresburgerPredTree subst (EqPred NomEq t1 t2) -- x ~ CmpNat a b
  | Just (con, [a, b]) <- splitTyConApp_maybe (substTy subst t2)
  , con `elem` [typeNatCmpTyCon]
  , Just cmp <- tyConAppTyCon_maybe (substTy subst t1) =
    let dic = [(promotedLTDataCon, (:<))
              ,(promotedEQDataCon, (:==))
              ,(promotedGTDataCon, (:>))
              ]
    in MaybeT (return $ lookup cmp dic)
       <*> toPresburgerExp subst a
       <*> toPresburgerExp subst b
toPresburgerPredTree subst (ClassPred con [t1, t2]) -- (n :: Nat) <= (m :: Nat)
  | typeNatLeqTyCon == classTyCon con
  , typeKind t1 `eqType` typeNatKind = (:<=) <$> toPresburgerExp subst t1 <*> toPresburgerExp subst t2
toPresburgerPredTree _ _ = mzero

toPresburgerExp :: TvSubst -> Type -> Machine Expr
toPresburgerExp dic ty = case substTy dic ty of
  TyVarTy t          -> return $ Var $ toName $ getKey $ getUnique t
  t@(TyConApp tc ts) -> body tc ts <|> Var . toName . getKey . getUnique <$> toVar t
  LitTy (NumTyLit n) -> return (K n)
  LitTy _            -> mzero
  t                  -> Var . toName . getKey .getUnique <$> toVar t
  where
    body tc ts =
      let step con op
            | tc == con, [tl, tr] <- ts =
              op <$> toPresburgerExp dic tl <*> toPresburgerExp dic tr
            | otherwise = mzero
      in case ts of
        [tl, tr] | tc == typeNatMulTyCon ->
          case (simpleExp tl, simpleExp tr) of
            (LitTy (NumTyLit n), LitTy (NumTyLit m)) -> return $ K $ n * m
            (LitTy (NumTyLit n), x) -> (:*) <$> pure n <*> toPresburgerExp dic x
            (x, LitTy (NumTyLit n)) -> (:*) <$> pure n <*> toPresburgerExp dic x
            _ -> mzero
        _ ->  asum [ step con op
                   | (con, op) <- [(typeNatAddTyCon, (:+)), (typeNatSubTyCon, (:-))]]

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

type ParseEnv = M.Map TypeEq TyVar

type Machine = MaybeT (StateT ParseEnv TcPluginM)

runMachine :: Machine a -> TcPluginM (Maybe a)
runMachine act = do
  (ma, dic) <- runStateT (runMaybeT act) M.empty
  forM_ (M.toList dic) $ \(TypeEq ty, var) ->
    newWanted undefined $ mkPrimEqPredRole Nominal (mkTyVarTy var) ty
  return ma

toVar :: Type -> Machine TyVar
toVar ty = gets (M.lookup (TypeEq ty)) >>= \case
  Just v -> return v
  Nothing -> do
    v <- lift $ lift $ newFlexiTyVar $ typeKind ty
    modify $ M.insert (TypeEq ty) v
    return v
