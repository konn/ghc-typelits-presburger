{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
-- | Since 0.3.0.0
module GHC.TypeLits.Presburger.Types
  ( pluginWith,
    defaultTranslation,
    Translation (..),
    ParseEnv,
    Machine,
    module Data.Integer.SAT,
  )
where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Monad (forM_, guard, mzero, unless)
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.RWS.Strict (runRWS, tell)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Char (isDigit)
import Data.Foldable (asum)
import Data.Integer.SAT (Expr (..), Prop (..), PropSet, assert, checkSat, noProps, toName)
import qualified Data.Integer.SAT as SAT
import Data.List (nub)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
  ( catMaybes,
    fromMaybe,
    isNothing,
    listToMaybe,
    maybeToList,
  )
import Data.Reflection (Given, give, given)
import qualified Data.Set as Set
import GHC.TypeLits.Presburger.Compat
import qualified Data.Foldable as F

assert' :: Prop -> PropSet -> PropSet
assert' p ps = foldr assert ps (p : varPos)
  where
    varPos = [K 0 :<= Var i | i <- varsProp p]

data Proof = Proved | Disproved [(Int, Integer)]
  deriving (Read, Show, Eq, Ord)

isProved :: Proof -> Bool
isProved Proved = True
isProved _ = False

varsProp :: Prop -> [SAT.Name]
varsProp (p :|| q) = nub $ varsProp p ++ varsProp q
varsProp (p :&& q) = nub $ varsProp p ++ varsProp q
varsProp (Not p) = varsProp p
varsProp (e :== v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :/= v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :< v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :> v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :<= v) = nub $ varsExpr e ++ varsExpr v
varsProp (e :>= v) = nub $ varsExpr e ++ varsExpr v
varsProp _ = []

varsExpr :: Expr -> [SAT.Name]
varsExpr (e :+ v) = nub $ varsExpr e ++ varsExpr v
varsExpr (e :- v) = nub $ varsExpr e ++ varsExpr v
varsExpr (_ :* v) = varsExpr v
varsExpr (Negate e) = varsExpr e
varsExpr (Min l r) = nub $ varsExpr l ++ varsExpr r
varsExpr (Max l r) = nub $ varsExpr l ++ varsExpr r
varsExpr (Var i) = [i]
varsExpr (K _) = []
varsExpr (If p e v) = nub $ varsProp p ++ varsExpr e ++ varsExpr v
varsExpr (Div e _) = varsExpr e
varsExpr (Mod e _) = varsExpr e

data PluginMode
  = DisallowNegatives
  | AllowNegatives
  deriving (Read, Show, Eq, Ord)

pluginWith :: TcPluginM Translation -> Plugin
pluginWith trans =
  defaultPlugin
    { tcPlugin = Just . presburgerPlugin trans . procOpts
#if MIN_VERSION_ghc(8,6,0)
    , pluginRecompile = purePlugin
#endif
    }
  where
    procOpts opts
      | "allow-negated-numbers" `elem` opts = AllowNegatives
      | otherwise = DisallowNegatives

presburgerPlugin :: TcPluginM Translation -> PluginMode -> TcPlugin
presburgerPlugin trans mode =
  tracePlugin
    "typelits-presburger"
    TcPlugin
      { tcPluginInit = return ()
      , tcPluginSolve = decidePresburger mode trans
      , tcPluginStop = const $ return ()
      }

testIf :: PropSet -> Prop -> Proof
testIf ps q = maybe Proved Disproved $ checkSat (Not q `assert'` ps)

-- Replaces every subtraction with new constant,
-- adding order constraint.
handleSubtraction :: PluginMode -> Prop -> Prop
handleSubtraction AllowNegatives p = p
handleSubtraction DisallowNegatives p0 =
  let (p, _, w) = runRWS (loop p0) () Set.empty
   in foldr (:&&) p w
  where
    loop PTrue = return PTrue
    loop PFalse = return PFalse
    loop (q :|| r) = (:||) <$> loop q <*> loop r
    loop (q :&& r) = (:&&) <$> loop q <*> loop r
    loop (Not q) = Not <$> loop q
    loop (l :<= r) = (:<=) <$> loopExp l <*> loopExp r
    loop (l :< r) = (:<) <$> loopExp l <*> loopExp r
    loop (l :>= r) = (:>=) <$> loopExp l <*> loopExp r
    loop (l :> r) = (:>) <$> loopExp l <*> loopExp r
    loop (l :== r) = (:==) <$> loopExp l <*> loopExp r
    loop (l :/= r) = (:/=) <$> loopExp l <*> loopExp r

    withPositive pos = do
      dic <- get
      unless (Set.member pos dic) $ do
        modify $ Set.insert pos
        tell $ Set.fromList [pos :>= K 0]
      return pos

    loopExp e@(Negate _) = withPositive . Negate =<< loopExp e
    loopExp (l :- r) = do
      e <- (:-) <$> loopExp l <*> loopExp r
      withPositive e
    loopExp (l :+ r) = (:+) <$> loopExp l <*> loopExp r
    loopExp v@Var {} = return v
    loopExp (c :* e)
      | c > 0 = (c :*) <$> loopExp e
      | otherwise = (negate c :*) <$> loopExp (Negate e)
    loopExp (Min l r) = Min <$> loopExp l <*> loopExp r
    loopExp (Max l r) = Max <$> loopExp l <*> loopExp r
    loopExp (If p l r) = If <$> loop p <*> loopExp l <*> loopExp r
    loopExp (Mod l n) = Mod <$> loopExp l <*> pure n
    loopExp (Div l n) = Div <$> loopExp l <*> pure n
    loopExp e@(K _) = return e

data Translation = Translation
  { isEmpty :: [TyCon]
  , ordCond :: [TyCon]
  , isTrue :: [TyCon]
  , trueData :: [TyCon]
  , falseData :: [TyCon]
  , voids :: [TyCon]
  , tyEq :: [TyCon]
  , tyEqBool :: [TyCon]
  , tyEqWitness :: [TyCon]
  , tyNeqBool :: [TyCon]
  , natPlus :: [TyCon]
  , natMinus :: [TyCon]
  , natExp :: [TyCon]
  , natTimes :: [TyCon]
  , natLeq :: [TyCon]
  , natLeqBool :: [TyCon]
  , natGeq :: [TyCon]
  , natGeqBool :: [TyCon]
  , natLt :: [TyCon]
  , natLtBool :: [TyCon]
  , natGt :: [TyCon]
  , natGtBool :: [TyCon]
  , natMin :: [TyCon]
  , natMax :: [TyCon]
  , orderingLT :: [TyCon]
  , orderingGT :: [TyCon]
  , orderingEQ :: [TyCon]
  , natCompare :: [TyCon]
  , parsePred :: (Type -> Machine Expr) -> Type -> Machine Prop
  , parseExpr :: (Type -> Machine Expr) -> Type -> Machine Expr
  }

instance Semigroup Translation where
  l <> r =
    Translation
      { isEmpty = isEmpty l <> isEmpty r
      , isTrue = isTrue l <> isTrue r
      , voids = voids l <> voids r
      , tyEq = tyEq l <> tyEq r
      , tyEqBool = tyEqBool l <> tyEqBool r
      , tyEqWitness = tyEqWitness l <> tyEqWitness r
      , tyNeqBool = tyNeqBool l <> tyNeqBool r
      , natPlus = natPlus l <> natPlus r
      , natMinus = natMinus l <> natMinus r
      , natTimes = natTimes l <> natTimes r
      , natExp = natExp l <> natExp r
      , natLeq = natLeq l <> natLeq r
      , natGeq = natGeq l <> natGeq r
      , natLt = natLt l <> natLt r
      , natGt = natGt l <> natGt r
      , natLeqBool = natLeqBool l <> natLeqBool r
      , natGeqBool = natGeqBool l <> natGeqBool r
      , natLtBool = natLtBool l <> natLtBool r
      , natGtBool = natGtBool l <> natGtBool r
      , orderingLT = orderingLT l <> orderingLT r
      , orderingGT = orderingGT l <> orderingGT r
      , orderingEQ = orderingEQ l <> orderingEQ r
      , natCompare = natCompare l <> natCompare r
      , trueData = trueData l <> trueData r
      , falseData = falseData l <> falseData r
      , parsePred = \f ty -> parsePred l f ty <|> parsePred r f ty
      , parseExpr = \toE -> (<|>) <$> parseExpr l toE <*> parseExpr r toE
      , natMin = natMin l <> natMin r
      , natMax = natMax l <> natMax r
      , ordCond = ordCond l <> ordCond r
      }

instance Monoid Translation where
  mempty =
    Translation
      { isEmpty = mempty
      , isTrue = mempty
      , tyEq = mempty
      , tyEqBool = mempty
      , tyEqWitness = mempty
      , tyNeqBool = mempty
      , voids = mempty
      , natPlus = mempty
      , natMinus = mempty
      , natTimes = mempty
      , natExp = mempty
      , natLeq = mempty
      , natGeq = mempty
      , natLt = mempty
      , natGt = mempty
      , natLeqBool = mempty
      , natGeqBool = mempty
      , natLtBool = mempty
      , natGtBool = mempty
      , orderingLT = mempty
      , orderingGT = mempty
      , orderingEQ = mempty
      , natCompare = mempty
      , trueData = []
      , falseData = []
      , parsePred = const $ const mzero
      , parseExpr = const $ const mzero
      , natMin = mempty
      , natMax = mempty
      , ordCond = mempty
      }

decidePresburger :: PluginMode -> TcPluginM Translation -> () -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
decidePresburger _ genTrans _ gs [] [] = do
  tcPluginTrace "pres: Started givens with: " (ppr $ map (ctEvPred . ctEvidence) gs)
  trans <- genTrans
  give trans $ do
    ngs <- mapM (\a -> runMachine $ (,) a <$> toPresburgerPred (deconsPred a)) gs
    let givens = catMaybes ngs
        prems0 = map snd givens
        prems = foldr assert' noProps prems0
    if isNothing (checkSat prems)
      then return $ TcPluginContradiction gs
      else return $ TcPluginOk [] []
decidePresburger mode genTrans _ gs _ds ws = do
  trans <- genTrans
  give trans $ do
    gs' <- normaliseGivens gs
    let subst = mkSubstitution gs'
    tcPluginTrace "pres: Current subst" (ppr subst)
    tcPluginTrace "pres: wanteds" $ ppr $ map (subsType subst . deconsPred . subsCt subst) ws
    tcPluginTrace "pres: givens" $ ppr $ map (subsType subst . deconsPred) gs
    tcPluginTrace "pres: deriveds" $ ppr $ map deconsPred _ds
    (prems, wants, prems0) <- do
      wants <-
        catMaybes
          <$> mapM
            ( \ct ->
                runMachine $
                  (,) ct
                    <$> toPresburgerPred
                      ( subsType subst $
                          deconsPred $ subsCt subst ct
                      )
            )
            (filter (isWanted . ctEvidence) ws)

      resls <-
        mapM
          (runMachine . toPresburgerPred . subsType subst . deconsPred)
          gs
      let prems = foldr assert' noProps $ catMaybes resls
      return (prems, map (second $ handleSubtraction mode) wants, catMaybes resls)
    let solved = map fst $ filter (isProved . testIf prems . snd) wants
        coerced =
          [ (evByFiat "ghc-typelits-presburger" t1 t2, ct)
          | ct <- solved
          , EqPred NomEq t1 t2 <- return (classifyPredType $ deconsPred ct)
          ]
    tcPluginTrace "pres: final premises" (text $ show prems0)
    tcPluginTrace "pres: final goals" (text $ show $ map snd wants)
    case testIf prems (foldr ((:&&) . snd) PTrue wants) of
      Proved -> do
        tcPluginTrace "pres: Proved" (text $ show $ map snd wants)
        tcPluginTrace "pres: ... with coercions" (ppr coerced)
        return $ TcPluginOk coerced []
      Disproved wit -> do
        tcPluginTrace "pres: Failed! " (text $ show wit)
        return $ TcPluginContradiction $ map fst wants

eqReasoning :: FastString
eqReasoning = fsLit "equational-reasoning"

defaultTranslation :: TcPluginM Translation
defaultTranslation = do
  packs <- preloadedUnitsM
  let eqThere = fromMaybe False $
        listToMaybe $ do
          pname <- packs
          rest <-
            maybeToList $
              L.stripPrefix "equational-reasoning-" $ unpackFS pname
          pure $ null rest || isDigit (head rest)
  (isEmpties, isTrues) <-
    if eqThere
      then do
        tcPluginTrace "pres: equational-reasoning activated!" $ ppr ()
        emd <- lookupModule (mkModuleName "Proof.Propositional.Empty") eqReasoning
        pmd <- lookupModule (mkModuleName "Proof.Propositional") eqReasoning

        emptyClsTyCon <- classTyCon <$> (tcLookupClass =<< lookupOrig emd (mkTcOcc "Empty"))
        isTrueCon_ <- tcLookupTyCon =<< lookupOrig pmd (mkTcOcc "IsTrue")
        pure ([emptyClsTyCon], [isTrueCon_])
      else do
        tcPluginTrace "pres: No equational-reasoning found." $ ppr ()
        pure ([], [])

  eqTyCon_ <- getEqTyCon
  eqBoolTyCon <- tcLookupTyCon =<< lookupOrig dATA_TYPE_EQUALITY (mkTcOcc "==")
  eqWitCon_ <- getEqWitnessTyCon
  vmd <- lookupModule (mkModuleName "Data.Void") (fsLit "base")
  voidTyCon <- tcLookupTyCon =<< lookupOrig vmd (mkTcOcc "Void")
  nLeq <- tcLookupTyCon =<< lookupTyNatPredLeq
  tyLeqB <- lookupTyNatBoolLeq
  mTyLtP <- lookupTyNatPredLt
  mTyLtB <- lookupTyNatBoolLt
  mTyGeqP <- lookupTyNatPredGeq
  mTyGeqB <- lookupTyNatBoolGeq
  mTyGtP <- lookupTyNatPredGt
  mTyGtB <- lookupTyNatBoolGt
  mOrdCond <- mOrdCondTyCon
  mtyGenericCompare <- lookupTyGenericCompare
  let trans =
        mempty
          { isEmpty = isEmpties
          , tyEq = [eqTyCon_]
          , ordCond = F.toList mOrdCond
          , tyEqWitness = [eqWitCon_]
          , tyEqBool = [eqBoolTyCon]
          , isTrue = isTrues
          , voids = [voidTyCon]
          , natMinus = [typeNatSubTyCon]
          , natPlus = [typeNatAddTyCon]
          , natTimes = [typeNatMulTyCon]
          , natExp = [typeNatExpTyCon]
          , falseData = [promotedFalseDataCon]
          , trueData = [promotedTrueDataCon]
          , natLeqBool = [tyLeqB]
          , natLeq = [nLeq]
          , natGeqBool = F.toList mTyGeqB
          , natGeq = F.toList mTyGeqP
          , natGtBool = F.toList mTyGtB
          , natGt = F.toList mTyGtP
          , natLtBool = F.toList mTyLtB
          , natLt = F.toList mTyLtP
          , natCompare = typeNatCmpTyCon : F.toList mtyGenericCompare
          , orderingEQ = [promotedEQDataCon]
          , orderingLT = [promotedLTDataCon]
          , orderingGT = [promotedGTDataCon]
          }
  tcPluginTrace "Final translation: " (ppr mTyGeqB)
  pure trans

(<=>) :: Prop -> Prop -> Prop
p <=> q = (p :&& q) :|| (Not p :&& Not q)

withEv :: Ct -> (EvTerm, Ct)
withEv ct =
  case classifyPredType (deconsPred ct) of
    EqPred _ t1 t2 -> (evByFiat "ghc-typelits-presburger" t1 t2, ct)
    _ -> error $ "UnknownPredEv: " <> showSDocUnsafe (ppr ct)

orderingDic :: Given Translation => [(TyCon, Expr -> Expr -> Prop)]
orderingDic =
  [(lt, (:<)) | lt <- orderingLT given]
  ++ [(eq, (:==)) | eq <- orderingEQ given]
    ++ [(gt, (:>)) | gt <- orderingGT given]

deconsPred :: Ct -> Type
deconsPred = ctEvPred . ctEvidence

toPresburgerPred :: Given Translation => Type -> Machine Prop
toPresburgerPred (TyConApp con (lastN 2 -> [t1, t2]))
  | con `elem` (natLeq given ++ natLeqBool given) =
    (:<=) <$> toPresburgerExp t1 <*> toPresburgerExp t2
toPresburgerPred ty
  | Just (con, []) <- splitTyConApp_maybe ty
    , con `elem` trueData given =
    return PTrue
  | Just (con, []) <- splitTyConApp_maybe ty
    , con `elem` falseData given =
    return PFalse
  | cls@(EqPred NomEq _ _) <- classifyPredType ty =
    toPresburgerPredTree cls
  | isEqPred ty = toPresburgerPredTree $ classifyPredType ty
  | Just (con, [l, r]) <- splitTyConAppLastBin ty -- l ~ r
    , con `elem` (tyEq given ++ tyEqBool given) =
    toPresburgerPredTree $ EqPred NomEq l r
  | Just (con, [_k, l, r]) <- splitTyConApp_maybe ty -- l (:~: {k}) r
    , con `elem` tyEqWitness given =
    toPresburgerPredTree $ EqPred NomEq l r
  | Just (con, [l]) <- splitTyConApp_maybe ty -- Empty l => ...
    , con `elem` isEmpty given =
    Not <$> toPresburgerPred l
  | Just (con, [l]) <- splitTyConApp_maybe ty -- IsTrue l =>
    , con `elem` isTrue given =
    toPresburgerPred l
  | Just (con, [t1, t2]) <- splitTyConAppLastBin ty
    , typeKind t1 `eqType` typeNatKind
    , typeKind t2 `eqType` typeNatKind 
    , Just p <- lookup con binPropDic =
      p <$> toPresburgerExp t1 <*> toPresburgerExp t2
  | Just DataCond{..} <- parseOrdCond ty = do -- OrdCond
      fromCondCases condCases
          <$> toPresburgerExp lhs
          <*> toPresburgerExp rhs
  | otherwise = parsePred given toPresburgerExp ty

splitTyConAppLastBin :: Type -> Maybe (TyCon, [Type])
splitTyConAppLastBin t = do
  (con, ts) <- splitTyConApp_maybe t
  let !n = length ts
  guard $ n >= 2
  return (con, drop (n - 2) ts)

data DataCond = DataCond { lhs, rhs :: Type, condCases :: CondCases }

data CondCases = CondCases { ltCase, eqCase, gtCase :: Bool }
  deriving (Show, Eq, Ord)

fromCondCases :: CondCases -> Expr -> Expr -> Prop
fromCondCases (CondCases True False False) = (:<)
fromCondCases (CondCases False True False) = (:==)
fromCondCases (CondCases False False True) = (:>)
fromCondCases (CondCases True True False) = (:<=)
fromCondCases (CondCases True False True) = (:/=)
fromCondCases (CondCases True True True) = const $ const PTrue
fromCondCases (CondCases False True True) = (:>=)
fromCondCases (CondCases False False False) = const $ const PFalse

parseOrdCond :: Given Translation => Type -> Maybe DataCond
parseOrdCond ty = do
  (con, lastN 4 -> [cmp, ltTy, eqTy, gtTy]) <- splitTyConApp_maybe ty
  guard $ con `elem` ordCond given
  (cmpCon, lastN 2 -> [lhs, rhs]) <- splitTyConApp_maybe  cmp
  guard $ cmpCon `elem` natCompare given
  ltCase <- decodeTyBool ltTy
  eqCase <- decodeTyBool eqTy
  gtCase <- decodeTyBool gtTy
  let  condCases = CondCases{..}
  pure DataCond{..}

decodeTyBool :: Type -> Maybe Bool
decodeTyBool ty = do
  con <- tyConAppTyCon_maybe ty
  (True <$ guard (con == promotedTrueDataCon))
    <|> (False <$ guard (con == promotedFalseDataCon))


toPresburgerPredTree :: Given Translation => PredTree -> Machine Prop
toPresburgerPredTree (EqPred NomEq p false) -- P ~ 'False <=> Not P ~ 'True
  | maybe False (`elem` falseData given) $ tyConAppTyCon_maybe false =
    Not <$> toPresburgerPred p
toPresburgerPredTree (EqPred NomEq p b) -- (n :<=? m) ~ 'b
  | Just isTrue <- decodeTyBool b
    , Just (con, [t1, t2]) <- splitTyConAppLastBin p
    , con `elem` natLeqBool given =
    if isTrue 
      then (:<=) <$> toPresburgerExp t1 <*> toPresburgerExp t2
       else (:>) <$> toPresburgerExp t1 <*> toPresburgerExp t2
toPresburgerPredTree (EqPred NomEq p b) -- (n :<? m) ~ b
  | Just isTrue <- decodeTyBool b
    , Just (con, [t1, t2]) <- splitTyConAppLastBin p
    , con `elem` natLtBool given =
    if isTrue 
      then (:<) <$> toPresburgerExp t1 <*> toPresburgerExp t2
       else (:>=) <$> toPresburgerExp t1 <*> toPresburgerExp t2
toPresburgerPredTree (EqPred NomEq p b) -- (n :>? m) ~ b
  | Just isTrue <- decodeTyBool b
    , Just (con, [t1, t2]) <- splitTyConAppLastBin p
    , con `elem` natGtBool given =
    if isTrue 
      then (:>) <$> toPresburgerExp t1 <*> toPresburgerExp t2
       else (:<=) <$> toPresburgerExp t1 <*> toPresburgerExp t2
toPresburgerPredTree (EqPred NomEq p b) -- (n :>=? m) ~ b
  | Just isTrue <- decodeTyBool b
    , Just (con, [t1, t2]) <- splitTyConAppLastBin p
    , con `elem` natGeqBool given =
    if isTrue 
      then (:>=) <$> toPresburgerExp t1 <*> toPresburgerExp t2
       else (:<) <$> toPresburgerExp t1 <*> toPresburgerExp t2
toPresburgerPredTree (EqPred NomEq p b) 
  | maybe False (`elem` trueData given) $ tyConAppTyCon_maybe b =
    toPresburgerPred p
toPresburgerPredTree (EqPred NomEq b p) 
  | maybe False (`elem` trueData given) $ tyConAppTyCon_maybe b =
    toPresburgerPred p
toPresburgerPredTree (EqPred NomEq p q) -- (p :: Bool) ~ (q :: Bool)
  | typeKind p `eqType` mkTyConTy promotedBoolTyCon = do
    lift $ lift $ tcPluginTrace "pres: EQBOOL:" $ ppr (p, q)
    (<=>) <$> toPresburgerPred p
      <*> toPresburgerPred q
toPresburgerPredTree (EqPred NomEq n m) -- (n :: Nat) ~ (m :: Nat)
  | typeKind n `eqType` typeNatKind =
    (:==) <$> toPresburgerExp n
      <*> toPresburgerExp m
toPresburgerPredTree (EqPred _ t1 t2) -- CmpNat a b ~ CmpNat c d
  | Just (con, lastTwo -> [a, b]) <- splitTyConAppLastBin t1
    , Just (con', lastTwo -> [c, d]) <- splitTyConAppLastBin t2
    , con `elem` natCompare given
    , con' `elem` natCompare given =
    (<=>) <$> ((:<) <$> toPresburgerExp a <*> toPresburgerExp b)
      <*> ((:<) <$> toPresburgerExp c <*> toPresburgerExp d)
toPresburgerPredTree (EqPred NomEq t1 t2) -- CmpNat a b ~ x
  | Just (con, lastTwo -> [a, b]) <- splitTyConAppLastBin t1
    , con `elem` natCompare given
    , Just cmp <- tyConAppTyCon_maybe t2 =
    MaybeT (return $ lookup cmp orderingDic)
      <*> toPresburgerExp a
      <*> toPresburgerExp b
toPresburgerPredTree (EqPred NomEq t1 t2) -- x ~ CmpNat a b
  | Just (con, lastTwo -> [a, b]) <- splitTyConAppLastBin t2
    , con `elem` natCompare given
    , Just cmp <- tyConAppTyCon_maybe t1 =
    MaybeT (return $ lookup cmp orderingDic)
      <*> toPresburgerExp a
      <*> toPresburgerExp b
toPresburgerPredTree (EqPred NomEq cond p) 
  -- OrdCond (CmpNat _ _) lt eq gt ~ ? (for GHC >= 9.2)
  | Just DataCond{..} <- parseOrdCond cond = do
    body <- fromCondCases condCases
          <$> toPresburgerExp lhs
          <*> toPresburgerExp rhs
    maybe ((body <=>) <$> toPresburgerPred p) 
      (\q -> if q then pure body else pure $ Not body)
      $ decodeTyBool p
  -- ? ~ OrdCond (CmpNat _ _) lt eq gt  (for GHC >= 9.2)
  | Just DataCond{..} <- parseOrdCond p = do
    body <- fromCondCases condCases
          <$> toPresburgerExp lhs
          <*> toPresburgerExp rhs
    maybe ((body <=>) <$> toPresburgerPred cond) 
      (\q -> if q then pure body else pure $ Not body)
      $ decodeTyBool cond
toPresburgerPredTree (ClassPred con ts)
  -- (n :: Nat) (<=| < | > | >= | == | /=) (m :: Nat)
  | [t1, t2] <- lastN 2 ts
    , typeKind t1 `eqType` typeNatKind
    , typeKind t2 `eqType` typeNatKind =
    let p = lookup (classTyCon con) binPropDic
     in MaybeT (return p) <*> toPresburgerExp t1 <*> toPresburgerExp t2
toPresburgerPredTree _ = mzero

binPropDic :: Given Translation => [(TyCon, Expr -> Expr -> Prop)]
binPropDic =
  [(n, (:<)) | n <- natLt given ++ natLtBool given]
  ++ [(n, (:>)) | n <- natGt given ++ natGtBool given]
    ++ [(n, (:<=)) | n <- natLeq given ++ natLeqBool given]
    ++ [(n, (:>=)) | n <- natGeq given ++ natGeqBool given]
    ++ [(n, (:==)) | n <- tyEq given ++ tyEqBool given]
    ++ [(n, (:/=)) | n <- tyNeqBool given]
    ++ [(n, (:==)) | n <- tyEqBool given]

toPresburgerExp :: Given Translation => Type -> Machine Expr
toPresburgerExp ty = case ty of
  TyVarTy t -> return $ Var $ toName $ getKey $ getUnique t
  TyConApp tc (lastN 4 -> [cmpNM, l, e, g])
    | tc `elem` ordCond given
    , TyConApp cmp (lastN 2 -> [n, m]) <- cmpNM
    , cmp `elem` natCompare given
    , all ((`elem` [TypeEq n, TypeEq m]) . TypeEq) [l,e,g]
    -> decodeMinMax n m l e g
  t@(TyConApp tc ts) ->
    parseExpr given toPresburgerExp ty
      <|> body tc ts
      <|> Var . toName . getKey . getUnique <$> toVar t
  LitTy (NumTyLit n) -> return (K n)
  LitTy _ -> mzero
  t ->
    parseExpr given toPresburgerExp ty
      <|> Var . toName . getKey . getUnique <$> toVar t
  where
    body tc ts =
      let step con op
            | tc == con
              , [tl, tr] <- lastTwo ts =
              op <$> toPresburgerExp tl <*> toPresburgerExp tr
            | otherwise = mzero
       in case ts of
            [tl, tr] | tc `elem` natTimes given ->
              case (simpleExp tl, simpleExp tr) of
                (LitTy (NumTyLit n), LitTy (NumTyLit m)) -> return $ K $ n * m
                (LitTy (NumTyLit n), x) -> (:*) n <$> toPresburgerExp x
                (x, LitTy (NumTyLit n)) -> (:*) n <$> toPresburgerExp x
                _ -> mzero
            _ ->
              asum $
                [ step con (:+)
                | con <- natPlus given
                ]
                  ++ [ step con (:-)
                     | con <- natMinus given
                     ]
                  ++ [ step con Min
                     | con <- natMin given
                     ]
                  ++ [ step con Max
                     | con <- natMin given
                     ]

decodeMinMax :: Given Translation => Type -> Type -> Type -> Type -> Type -> Machine Expr
decodeMinMax n m lt eq gt 
  | lt `eqType`  n && eq `eqType`  n && gt `eqType` m = 
    Min <$> toPresburgerExp n <*> toPresburgerExp m
  | lt `eqType`  n && eq `eqType`  m && gt `eqType` m = 
    Min <$> toPresburgerExp n <*> toPresburgerExp m
  | lt `eqType` m && eq `eqType`  m && gt `eqType` n = 
    Max <$> toPresburgerExp n <*> toPresburgerExp m
  | lt `eqType` m && eq `eqType`  n && gt `eqType` n = 
    Max <$> toPresburgerExp n <*> toPresburgerExp m
  | otherwise = mzero

-- simplTypeCmp :: Type -> Type

lastTwo :: [a] -> [a]
lastTwo = drop <$> subtract 2 . length <*> id
lastN :: Int -> [a] -> [a]
lastN n = drop <$> subtract n . length <*> id

simpleExp :: Given Translation => Type -> Type
simpleExp (AppTy t1 t2) = AppTy (simpleExp t1) (simpleExp t2)
#if MIN_VERSION_ghc(9,0,0)
simpleExp (FunTy f m t1 t2) = FunTy f m (simpleExp t1) (simpleExp t2)
#else
#if MIN_VERSION_ghc(8,10,1)
simpleExp (FunTy f t1 t2) = FunTy f (simpleExp t1) (simpleExp t2)
#else
simpleExp (FunTy t1 t2) = FunTy (simpleExp t1) (simpleExp t2)
#endif
#endif
simpleExp (ForAllTy t1 t2) = ForAllTy t1 (simpleExp t2)
simpleExp (TyConApp tc (lastTwo -> ts)) =
  fromMaybe (TyConApp tc (map simpleExp ts)) $
    asum
      ( map simpler $
          [(c, (+)) | c <- natPlus given]
          ++ [(c, (-)) | c <- natMinus given]
            ++ [(c, (*)) | c <- natTimes given]
            ++ [(c, (^)) | c <- natExp given]
      )
  where
    simpler (con, op)
      | con == tc
        , [tl, tr] <- map simpleExp ts =
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
toVar ty =
  gets (M.lookup (TypeEq ty)) >>= \case
    Just v -> return v
    Nothing -> do
      v <- lift $ lift $ newFlexiTyVar $ typeKind ty
      modify $ M.insert (TypeEq ty) v
      return v
