{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Singletons.TypeNats.Presburger
  ( plugin,
    singletonTranslation,
  )
where

import Control.Monad
import Control.Monad.Trans (MonadTrans (lift))
import Data.Reflection (Given, give, given)
import GHC.TypeLits.Presburger.Compat
import GHC.TypeLits.Presburger.Types

plugin :: Plugin
plugin =
  pluginWith $
    (<>) <$> defaultTranslation <*> singletonTranslation

data SingletonCons = SingletonCons
  { singNatLeq :: TyCon
  , singNatGeq :: TyCon
  , singNatLt :: TyCon
  , singNatGt :: TyCon
  , singNatPlus :: TyCon
  , singNatMinus :: TyCon
  , singNatTimes :: TyCon
  , singNatCompare :: TyCon
  , singTrueSym0 :: TyCon
  , singFalseSym0 :: TyCon
  , caseNameForSingLeq :: TyCon
  , caseNameForSingGeq :: TyCon
  , caseNameForSingLt :: TyCon
  , caseNameForSingGt :: TyCon
  , singMin :: TyCon
  , singMax :: TyCon
  , caseNameForMin :: TyCon
  , caseNameForMax :: TyCon
  }

singletonTranslation ::
  TcPluginM Translation
singletonTranslation = toTranslation <$> genSingletonCons

toTranslation ::
  SingletonCons -> Translation
toTranslation scs@SingletonCons {..} =
  give scs $
    mempty
      { natLeqBool = [singNatLeq]
      , natGeqBool = [singNatGeq]
      , natLtBool = [singNatLt]
      , natGtBool = [singNatGt]
      , natCompare = [singNatCompare]
      , natPlus = [singNatPlus]
      , natMinus = [singNatMinus]
      , natTimes = [singNatTimes]
      , parsePred = parseSingPred
      , parseExpr = parseSingExpr
      , trueData = [singTrueSym0]
      , natMin = [singMin]
      , natMax = [singMax]
      , falseData = [singFalseSym0]
      }

singPackage :: FastString
#if defined(MIN_VERISION_singletons_base)
singPackage = "singletons-base"
#else
singPackage = "singletons"
#endif

ordModName, numModName, prelInstName :: ModuleName
#if defined(SINGLETONS_BASE)
ordModName = mkModuleName "Data.Ord.Singletons"
numModName = mkModuleName "GHC.Num.Singletons"
prelInstName = mkModuleName "Data.Singletons.Base.Instances"
#else
ordModName = mkModuleName "Data.Singletons.Prelude.Ord"
numModName = mkModuleName "Data.Singletons.Prelude.Num"
prelInstName = mkModuleName "Data.Singletons.Prelude.Instances"
#endif

genSingletonCons :: TcPluginM SingletonCons
genSingletonCons = do
  singletonOrd <- lookupModule ordModName singPackage
  let singUnit = moduleUnit' singletonOrd
      prel = mkModule singUnit prelInstName
      singletonsNum = mkModule singUnit numModName
  singTrueSym0 <- tcLookupTyCon =<< lookupOrig prel (mkTcOcc "TrueSym0")
  singFalseSym0 <- tcLookupTyCon =<< lookupOrig prel (mkTcOcc "FalseSym0")
#if MIN_VERSION_singletons(2,4,1)
  singNatLeq <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "<=")
  singNatLt <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "<")
  singNatGeq <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc ">=")
  singNatGt <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc ">")
  singNatPlus <- tcLookupTyCon =<< lookupOrig singletonsNum (mkTcOcc "+")
  singNatTimes <- tcLookupTyCon =<< lookupOrig singletonsNum (mkTcOcc "*")
  singNatMinus <- tcLookupTyCon =<< lookupOrig singletonsNum (mkTcOcc "-")
#else
  singNatLeq <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc ":<=")
  singNatLt <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc ":<")
  singNatGeq <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc ":>=")
  singNatGt <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc ":>")
  singNatPlus <- tcLookupTyCon =<< lookupOrig singletonsNum (mkTcOcc ":+")
  singNatTimes <- tcLookupTyCon =<< lookupOrig singletonsNum (mkTcOcc ":*")
  singNatMinus <- tcLookupTyCon =<< lookupOrig singletonsNum (mkTcOcc ":-")
#endif
  singMin <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "Min")
  singMax <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "Max")
  caseNameForSingLeq <- getCaseNameForSingletonBinRel singNatLeq
  caseNameForSingLt <- getCaseNameForSingletonBinRel singNatLt
  caseNameForSingGeq <- getCaseNameForSingletonBinRel singNatGeq
  caseNameForSingGt <- getCaseNameForSingletonBinRel singNatGt
  caseNameForMin <- getCaseNameForSingletonBinOp singMin
  caseNameForMax <- getCaseNameForSingletonBinOp singMax
  singNatCompare <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "Compare")
  tcPluginTrace "pres: minMaxes" $
    ppr (singMin, singMax, caseNameForMin, caseNameForMax)
  return SingletonCons {..}

getCaseNameForSingletonBinOp :: TyCon -> TcPluginM TyCon
getCaseNameForSingletonBinOp con = do
  let vars = [typeNatKind, LitTy (NumTyLit 0), LitTy (NumTyLit 0)]
  tcPluginTrace "matching... for " (ppr con)
  Just (appTy0, [n, b, bdy, r]) <- fmap (splitTyConApp . snd) <$> matchFam con vars
  let (appTy, args) = splitTyConApp bdy
  Just innermost <- fmap snd <$> matchFam appTy args
  Just (_, dat) <- matchFam appTy0 [n, b, innermost, r]
  Just dat' <- fmap snd <$> uncurry matchFam (splitTyConApp dat)
  let Just (con', _) = splitTyConApp_maybe dat'
  return con'

getCaseNameForSingletonBinRel :: TyCon -> TcPluginM TyCon
getCaseNameForSingletonBinRel con = do
  let vars = [typeNatKind, LitTy (NumTyLit 0), LitTy (NumTyLit 0)]
  tcPluginTrace "matching... for " (ppr con)
  Just (appTy0, [n, b, bdy, r]) <- fmap (splitTyConApp . snd) <$> matchFam con vars
  let (appTy, args) = splitTyConApp bdy
  Just innermost <- fmap snd <$> matchFam appTy args
  Just (_, dat) <- matchFam appTy0 [n, b, innermost, r]
  Just dat' <- fmap snd <$> uncurry matchFam (splitTyConApp dat)
  tcPluginTrace "matched. (orig, inner) = " (ppr (con, fst $ splitTyConApp dat'))
  return $ fst $ splitTyConApp dat'

lastTwo :: [a] -> [a]
lastTwo = drop <$> subtract 2 . length <*> id

parseSingExpr ::
  (Given SingletonCons) =>
  (Type -> Machine Expr) ->
  Type ->
  Machine Expr
parseSingExpr toE ty
  | Just (con, [_, l, r, _]) <- splitTyConApp_maybe ty
    , Just bin <- lookup con minLikeCaseDic = do
    lift $ lift $ tcPluginTrace "hit!" $ ppr (ty, con)
    bin <$> toE l <*> toE r
  | otherwise = do
    lift $ lift $ tcPluginTrace "I don't know how to read:" $ ppr (ty, splitTyConApp_maybe ty)
    mzero

minLikeCaseDic :: Given SingletonCons => [(TyCon, Expr -> Expr -> Expr)]
minLikeCaseDic =
  [ (caseNameForMin given, Min)
  , (caseNameForMax given, Max)
  ]

parseSingPred ::
  (Given SingletonCons) =>
  (Type -> Machine Expr) ->
  Type ->
  Machine Prop
parseSingPred toExp ty
  | isEqPred ty = parseSingPredTree toExp $ classifyPredType ty
  | Just (con, [_, _, _, _, cmpTy]) <- splitTyConApp_maybe ty
    , Just bin <- lookup con compCaseDic
    , Just (cmp, lastTwo -> [l, r]) <- splitTyConApp_maybe cmpTy
    , cmp `elem` [singNatCompare given, typeNatCmpTyCon] =
    bin <$> toExp l <*> toExp r
  | otherwise = do
    lift $ lift $ tcPluginTrace "pres: Miokuring" (ppr ty)
    mzero

compCaseDic :: Given SingletonCons => [(TyCon, Expr -> Expr -> Prop)]
compCaseDic =
  [ (caseNameForSingLeq given, (:<=))
  , (caseNameForSingLt given, (:<))
  , (caseNameForSingGeq given, (:>=))
  , (caseNameForSingGt given, (:>))
  ]

parseSingPredTree ::
  Given SingletonCons =>
  (Type -> Machine Expr) ->
  PredTree ->
  Machine Prop
parseSingPredTree toExp (EqPred NomEq p b) -- (n :<=? m) ~ 'True
  | Just promotedTrueDataCon == tyConAppTyCon_maybe b -- Singleton's <=...
    , Just (con, [_, _, _, _, cmpTy]) <- splitTyConApp_maybe p
    , Just bin <- lookup con compCaseDic
    , Just (cmp, lastTwo -> [l, r]) <- splitTyConApp_maybe cmpTy
    , cmp `elem` [singNatCompare given, typeNatCmpTyCon] =
    bin <$> toExp l <*> toExp r
  | Just promotedFalseDataCon == tyConAppTyCon_maybe b -- Singleton's <=...
    , Just (con, [_, _, _, _, cmpTy]) <- splitTyConApp_maybe p
    , Just bin <- lookup con compCaseDic
    , Just (cmp, lastTwo -> [l, r]) <- splitTyConApp_maybe cmpTy
    , cmp `elem` [singNatCompare given, typeNatCmpTyCon] =
    fmap Not . bin <$> toExp l <*> toExp r
parseSingPredTree _ _ = mzero
