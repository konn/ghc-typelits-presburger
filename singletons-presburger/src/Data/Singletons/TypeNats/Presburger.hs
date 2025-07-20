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

module Data.Singletons.TypeNats.Presburger (
  plugin,
  singletonTranslation,
) where

import Control.Monad
import Control.Monad.Trans (MonadTrans (lift))
import Data.Reflection (Given, give, given)
import GHC.Plugins (Outputable, (<+>))
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
  , caseNameForSingLeq :: ComparisonMethod
  , caseNameForSingGeq :: ComparisonMethod
  , caseNameForSingLt :: ComparisonMethod
  , caseNameForSingGt :: ComparisonMethod
  , singMin :: TyCon
  , singMax :: TyCon
  , caseNameForMin :: ComparisonMethod
  , caseNameForMax :: ComparisonMethod
  }

data ComparisonMethod = CaseFun TyCon | DirectTFHelper TyCon
  deriving (Eq)

instance Outputable ComparisonMethod where
  ppr (CaseFun con) = text "CaseFun" <+> ppr con
  ppr (DirectTFHelper con) = text "DirectTFHelper" <+> ppr con

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
singPackage = "singletons-base"

ordModName, numModName, prelInstName :: ModuleName
ordModName = mkModuleName "Data.Ord.Singletons"
numModName = mkModuleName "GHC.Num.Singletons"
prelInstName = mkModuleName "Data.Singletons.Base.Instances"

genSingletonCons :: TcPluginM SingletonCons
genSingletonCons = do
  tcPluginTrace "sing: Start singleton consing" ""
  singletonOrd <- lookupModule ordModName singPackage
  tcPluginTrace "sing: singletonOrd" $ ppr singletonOrd
  let singUnit = moduleUnit' singletonOrd
      prel = mkModule singUnit prelInstName
      singletonsNum = mkModule singUnit numModName
  singTrueSym0 <- tcLookupTyCon =<< lookupOrig prel (mkTcOcc "TrueSym0")
  tcPluginTrace "sing: singTrueSym0" $ ppr singTrueSym0
  singFalseSym0 <- tcLookupTyCon =<< lookupOrig prel (mkTcOcc "FalseSym0")
  tcPluginTrace "sing: singFalseSym0" $ ppr singFalseSym0
  singNatLeq <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "<=")
  tcPluginTrace "sing: singNatLeq" $ ppr singNatLeq
  singNatLt <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "<")
  tcPluginTrace "sing: singNatLt" $ ppr singNatLt
  singNatGeq <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc ">=")
  tcPluginTrace "sing: singNatGeq" $ ppr singNatGeq
  singNatGt <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc ">")
  tcPluginTrace "sing: singNatGt" $ ppr singNatGt
  singNatPlus <- tcLookupTyCon =<< lookupOrig singletonsNum (mkTcOcc "+")
  tcPluginTrace "sing: singNatPlus" $ ppr singNatPlus
  singNatTimes <- tcLookupTyCon =<< lookupOrig singletonsNum (mkTcOcc "*")
  tcPluginTrace "sing: singNatTimes" $ ppr singNatTimes
  singNatMinus <- tcLookupTyCon =<< lookupOrig singletonsNum (mkTcOcc "-")
  tcPluginTrace "sing: singNatMinus" $ ppr singNatMinus
  singMin <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "Min")
  tcPluginTrace "sing: singNatMin" $ ppr singMin
  singMax <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "Max")
  tcPluginTrace "sing: singNatMax" $ ppr singMax
  caseNameForSingLeq <- getCaseNameForSingletonBinRel singNatLeq
  tcPluginTrace "sing: caseNameForSingLeq" $ ppr caseNameForSingLeq
  caseNameForSingLt <- getCaseNameForSingletonBinRel singNatLt
  caseNameForSingGeq <- getCaseNameForSingletonBinRel singNatGeq
  caseNameForSingGt <- getCaseNameForSingletonBinRel singNatGt
  caseNameForMin <- getCaseNameForSingletonBinOp singMin
  caseNameForMax <- getCaseNameForSingletonBinOp singMax
  singNatCompare <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "Compare")
  tcPluginTrace "pres: minMaxes" $
    ppr (singMin, singMax, caseNameForMin, caseNameForMax)
  return SingletonCons {..}

getCaseNameForSingletonBinOp :: TyCon -> TcPluginM ComparisonMethod
#if MIN_VERSION_singletons_base(3,5,0)
getCaseNameForSingletonBinOp = getCaseNameForSingletonBinOpSingleton35
#else
getCaseNameForSingletonBinOp = getCaseNameForSingletonBinOpOldSingleton
#endif

getCaseNameForSingletonBinOpSingleton35 :: TyCon -> TcPluginM ComparisonMethod
getCaseNameForSingletonBinOpSingleton35 con = do
  let vars = [typeNatKind, LitTy (NumTyLit 0), LitTy (NumTyLit 0)]
  tcPluginTrace "matching... for " (ppr con)
  matched <- fmap splitTyConApp <$> matchFam' con vars
  tcPluginTrace "matched. " (ppr matched)
  Just (appTy0, [_, _, _]) <- fmap splitTyConApp <$> matchFam' con vars
  return $ DirectTFHelper appTy0

getCaseNameForSingletonBinOpOldSingleton :: TyCon -> TcPluginM ComparisonMethod
getCaseNameForSingletonBinOpOldSingleton con = do
  let vars = [typeNatKind, LitTy (NumTyLit 0), LitTy (NumTyLit 0)]
  tcPluginTrace "matching... for " (ppr con)
  Just (appTy0, [n, b, bdy, r]) <- fmap splitTyConApp <$> matchFam' con vars
  let (appTy, args) = splitTyConApp bdy
  Just innermost <- matchFam' appTy args
  Just dat <- matchFam' appTy0 [n, b, innermost, r]
  Just dat' <- uncurry matchFam' (splitTyConApp dat)
  tcPluginTrace "matched. (orig, inner) = " (ppr (con, fst $ splitTyConApp dat'))
  return $ CaseFun $ fst $ splitTyConApp dat'

getCaseNameForSingletonBinRel :: TyCon -> TcPluginM ComparisonMethod
#if MIN_VERSION_singletons_base(3,5,0)
getCaseNameForSingletonBinRel = getCaseNameForSingletonBinRelSingleton35
#else
getCaseNameForSingletonBinRel =getCaseNameForSingletonBinRelOldSingleton
#endif

getCaseNameForSingletonBinRelSingleton35 :: TyCon -> TcPluginM ComparisonMethod
getCaseNameForSingletonBinRelSingleton35 con = do
  let vars = [typeNatKind, LitTy (NumTyLit 0), LitTy (NumTyLit 0)]
  tcPluginTrace "matching... for " (ppr con)
  matched <- fmap splitTyConApp <$> matchFam' con vars
  tcPluginTrace "matched. " (ppr matched)
  Just (appTy0, [_, _, _]) <- fmap splitTyConApp <$> matchFam' con vars
  return $ DirectTFHelper appTy0

getCaseNameForSingletonBinRelOldSingleton :: TyCon -> TcPluginM ComparisonMethod
getCaseNameForSingletonBinRelOldSingleton con = do
  let vars = [typeNatKind, LitTy (NumTyLit 0), LitTy (NumTyLit 0)]
  tcPluginTrace "matching... for " (ppr con)
  Just (appTy0, [n, b, bdy, r]) <- fmap splitTyConApp <$> matchFam' con vars
  let (appTy, args) = splitTyConApp bdy
  Just innermost <- matchFam' appTy args
  Just dat <- matchFam' appTy0 [n, b, innermost, r]
  Just dat' <- uncurry matchFam' (splitTyConApp dat)
  tcPluginTrace "matched. (orig, inner)<= = " (ppr (con, fst $ splitTyConApp dat'))
  return $ CaseFun $ fst $ splitTyConApp dat'

lastTwo :: [a] -> [a]
lastTwo = drop <$> subtract 2 . length <*> id

parseSingExpr ::
  (Given SingletonCons) =>
  (Type -> Machine Expr) ->
  Type ->
  Machine Expr
parseSingExpr toE ty
  -- singletons >=3.5
  | Just (con, [l, r]) <- splitTyConApp_maybe ty
  , Just bin <- lookup (DirectTFHelper con) minLikeCaseDic = do
      lift $ lift $ tcPluginTrace "hit!" $ ppr (ty, con)
      bin <$> toE l <*> toE r
  -- singletons <3.5
  | Just (con, [_, l, r, _]) <- splitTyConApp_maybe ty
  , Just bin <- lookup (CaseFun con) minLikeCaseDic = do
      lift $ lift $ tcPluginTrace "hit!" $ ppr (ty, con)
      bin <$> toE l <*> toE r
  | otherwise = do
      lift $ lift $ tcPluginTrace "I don't know how to read:" $ ppr (ty, splitTyConApp_maybe ty)
      mzero

minLikeCaseDic :: (Given SingletonCons) => [(ComparisonMethod, Expr -> Expr -> Expr)]
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
  | Just (con, [l, r]) <- splitTyConApp_maybe ty
  , Just bin <- lookup (DirectTFHelper con) compCaseDic =
      bin <$> toExp l <*> toExp r
  | Just (con, [_, _, _, _, cmpTy]) <- splitTyConApp_maybe ty
  , Just bin <- lookup (CaseFun con) compCaseDic
  , Just (cmp, lastTwo -> [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singNatCompare given, typeNatCmpTyCon] =
      bin <$> toExp l <*> toExp r
  | otherwise = do
      lift $ lift $ tcPluginTrace "pres: Miokuring" (ppr ty)
      mzero

compCaseDic :: (Given SingletonCons) => [(ComparisonMethod, Expr -> Expr -> Prop)]
compCaseDic =
  [ (caseNameForSingLeq given, (:<=))
  , (caseNameForSingLt given, (:<))
  , (caseNameForSingGeq given, (:>=))
  , (caseNameForSingGt given, (:>))
  ]

parseSingPredTree ::
  (Given SingletonCons) =>
  (Type -> Machine Expr) ->
  PredTree ->
  Machine Prop
parseSingPredTree toExp (EqPred NomEq p b) -- (n :<=? m) ~ 'True

  -- singletons >=3.5
  | Just promotedTrueDataCon == tyConAppTyCon_maybe b -- Singleton's <=...
  , Just (con, [l, r]) <- splitTyConApp_maybe p
  , Just bin <- lookup (DirectTFHelper con) compCaseDic =
      bin <$> toExp l <*> toExp r
  | Just promotedFalseDataCon == tyConAppTyCon_maybe b -- Singleton's <=...
  , Just (con, [l, r]) <- splitTyConApp_maybe p
  , Just bin <- lookup (DirectTFHelper con) compCaseDic =
      fmap Not . bin <$> toExp l <*> toExp r
  -- singletons <3.5
  | Just promotedTrueDataCon == tyConAppTyCon_maybe b -- Singleton's <=...
  , Just (con, [_, _, _, _, cmpTy]) <- splitTyConApp_maybe p
  , Just bin <- lookup (CaseFun con) compCaseDic
  , Just (cmp, lastTwo -> [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singNatCompare given, typeNatCmpTyCon] =
      bin <$> toExp l <*> toExp r
  | Just promotedFalseDataCon == tyConAppTyCon_maybe b -- Singleton's <=...
  , Just (con, [_, _, _, _, cmpTy]) <- splitTyConApp_maybe p
  , Just bin <- lookup (CaseFun con) compCaseDic
  , Just (cmp, lastTwo -> [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singNatCompare given, typeNatCmpTyCon] =
      fmap Not . bin <$> toExp l <*> toExp r
parseSingPredTree _ _ = mzero
