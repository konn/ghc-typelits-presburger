{-# LANGUAGE CPP, DataKinds, FlexibleContexts, FlexibleInstances      #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, PatternGuards, RankNTypes #-}
{-# LANGUAGE RecordWildCards, TypeOperators, ViewPatterns             #-}
module Data.Singletons.TypeNats.Presburger
  (plugin, singletonTranslation
  ) where
import GHC.TypeLits.Presburger.Compat
import GHC.TypeLits.Presburger.Types

import Control.Monad
import Data.Reflection (Given, give, given)
import GHC             (mkModule, moduleUnitId)
import TcPluginM       (lookupOrig, matchFam)
import Type            (splitTyConApp)

plugin :: Plugin
plugin = pluginWith $
  (<>) <$> defaultTranslation <*> singletonTranslation

data SingletonCons
  = SingletonCons
      { singNatLeq         :: TyCon
      , singNatGeq         :: TyCon
      , singNatLt          :: TyCon
      , singNatGt          :: TyCon
      , singNatPlus        :: TyCon
      , singNatMinus       :: TyCon
      , singNatTimes       :: TyCon
      , singNatCompare     :: TyCon
      , singTrueSym0       :: TyCon
      , singFalseSym0      :: TyCon
      , caseNameForSingLeq :: TyCon
      , caseNameForSingGeq :: TyCon
      , caseNameForSingLt  :: TyCon
      , caseNameForSingGt  :: TyCon
      }

singletonTranslation
  :: TcPluginM Translation
singletonTranslation = toTranslation <$> genSingletonCons

toTranslation
  :: SingletonCons -> Translation
toTranslation scs@SingletonCons{..} =
  give scs $
    mempty
    { natLeqBool = [singNatLeq]
    , natGeqBool = [singNatGeq]
    , natLtBool  = [singNatLt]
    , natGtBool  = [singNatGt]
    , natCompare = [singNatCompare]
    , natPlus = [singNatPlus]
    , natMinus = [singNatMinus]
    , natTimes = [singNatTimes]
    , parsePred = parseSingPred
    , trueData = [singTrueSym0]
    , falseData = [singFalseSym0]
    }

genSingletonCons :: TcPluginM SingletonCons
genSingletonCons = do
  singletonOrd <- lookupModule (mkModuleName "Data.Singletons.Prelude.Ord") (fsLit "singletons")
  singletonsNum <- lookupModule (mkModuleName "Data.Singletons.Prelude.Num") (fsLit "singletons")
  let prel = mkModule (moduleUnitId singletonsNum) (mkModuleName "Data.Singletons.Prelude.Instances")
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
  caseNameForSingLeq <- getCaseNameForSingletonOp singNatLeq
  caseNameForSingLt <- getCaseNameForSingletonOp singNatLt
  caseNameForSingGeq <- getCaseNameForSingletonOp singNatGeq
  caseNameForSingGt <- getCaseNameForSingletonOp singNatGt
  singNatCompare <- tcLookupTyCon =<< lookupOrig singletonOrd (mkTcOcc "Compare")
  return SingletonCons{..}

getCaseNameForSingletonOp :: TyCon -> TcPluginM TyCon
getCaseNameForSingletonOp con = do
  let vars = [typeNatKind, LitTy (NumTyLit 0), LitTy (NumTyLit 0)]
  tcPluginTrace "matching... for " (ppr con)
  Just (appTy0, [n,b,bdy,r]) <- fmap (splitTyConApp . snd) <$> matchFam  con vars
  let (appTy, args) = splitTyConApp bdy
  Just innermost <- fmap snd <$> matchFam appTy args
  Just (_, dat) <- matchFam appTy0 [n,b,innermost,r]
  Just dat' <- fmap snd <$> uncurry matchFam (splitTyConApp dat)
  tcPluginTrace "matched. (orig, inner) = " (ppr (con, fst $ splitTyConApp dat'))
  return $ fst $ splitTyConApp dat'

lastTwo :: [a] -> [a]
lastTwo = drop <$> subtract 2 . length <*> id

parseSingPred
  :: (Given SingletonCons)
  => (Type -> Machine Expr) -> Type -> Machine Prop
parseSingPred toExp ty
  | isEqPred ty = parseSingPredTree toExp $ classifyPredType ty
  | Just (con, [_,_,_,_,cmpTy]) <- splitTyConApp_maybe ty
  , Just bin <- lookup con compCaseDic
  , Just (cmp, lastTwo -> [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singNatCompare given, typeNatCmpTyCon] =
      bin <$> toExp l <*> toExp r
  | otherwise = mzero

compCaseDic :: Given SingletonCons => [(TyCon, Expr -> Expr -> Prop)]
compCaseDic =
  [ (caseNameForSingLeq given, (:<=))
  , (caseNameForSingLt given, (:<))
  , (caseNameForSingGeq given, (:>=))
  , (caseNameForSingGt given, (:>))
  ]


parseSingPredTree
  :: Given SingletonCons
  => (Type -> Machine Expr)
  -> PredTree -> Machine Prop
parseSingPredTree toExp (EqPred NomEq p b)  -- (n :<=? m) ~ 'True
  | Just promotedTrueDataCon  == tyConAppTyCon_maybe b -- Singleton's <=...
  , Just (con, [_,_,_,_,cmpTy]) <- splitTyConApp_maybe p
  , Just bin <- lookup con compCaseDic
  , Just (cmp, lastTwo -> [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singNatCompare given, typeNatCmpTyCon] =
    bin <$> toExp l <*> toExp r
  | Just promotedFalseDataCon  == tyConAppTyCon_maybe b -- Singleton's <=...
  , Just (con, [_,_,_,_,cmpTy]) <- splitTyConApp_maybe p
  , Just bin <- lookup con compCaseDic
  , Just (cmp, lastTwo -> [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singNatCompare given, typeNatCmpTyCon] =
    fmap Not . bin <$> toExp l <*> toExp r
parseSingPredTree _ _ = mzero
