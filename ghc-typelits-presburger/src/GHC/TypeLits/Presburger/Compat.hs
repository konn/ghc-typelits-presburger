{-# LANGUAGE CPP, FlexibleInstances, PatternGuards, PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, ViewPatterns                     #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.TypeLits.Presburger.Compat (module GHC.TypeLits.Presburger.Compat) where
import Data.Function       (on)
import GHC.TcPluginM.Extra as GHC.TypeLits.Presburger.Compat (evByFiat, lookupModule, lookupName,
                                          tracePlugin)
import Data.Generics.Twins

#if MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Names as GHC.TypeLits.Presburger.Compat (gHC_TYPENATS, dATA_TYPE_EQUALITY)
import qualified GHC.Builtin.Names as Old
import GHC.Hs as GHC.TypeLits.Presburger.Compat (HsModule(..), NoExtField(..))
import GHC.Hs.ImpExp as GHC.TypeLits.Presburger.Compat (ImportDecl(..), ImportDeclQualifiedStyle(..))
import GHC.Hs.Extension as GHC.TypeLits.Presburger.Compat (GhcPs)
import GHC.Builtin.Types as GHC.TypeLits.Presburger.Compat
  ( boolTyCon,
    eqTyConName,
    promotedEQDataCon,
    promotedGTDataCon,
    promotedLTDataCon,
  )
import qualified GHC.Builtin.Types as TysWiredIn
import GHC.Builtin.Types.Literals as GHC.TypeLits.Presburger.Compat
import GHC.Core.Class as GHC.TypeLits.Presburger.Compat (className, classTyCon)
import GHC.Core.FamInstEnv as GHC.TypeLits.Presburger.Compat
import GHC.Core.Predicate as GHC.TypeLits.Presburger.Compat (EqRel (..), Pred (..), isEqPred, mkPrimEqPredRole)
import qualified GHC.Core.Predicate as Old (classifyPredType)
import GHC.Core.TyCo.Rep as GHC.TypeLits.Presburger.Compat (TyLit (NumTyLit), Type (..))
import GHC.Core.TyCon as GHC.TypeLits.Presburger.Compat
import qualified GHC.Core.Type as Old
import GHC.Core.Unify as Old (tcUnifyTy)
import GHC.Unit.Types (Module, UnitId, toUnitId)
import GHC.Unit.Types as GHC.TypeLits.Presburger.Compat (mkModule)
import GHC.Data.FastString as GHC.TypeLits.Presburger.Compat (FastString, fsLit, unpackFS)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Env.Types as GHC.TypeLits.Presburger.Compat (HscEnv (hsc_dflags))
#else
import GHC.Driver.Types as GHC.TypeLits.Presburger.Compat (HscEnv (hsc_dflags))
import GHC.Driver.Session (unitState)
#endif
import GHC.Plugins (InScopeSet, Outputable, emptyUFM, moduleUnit, Unit, Name)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Hs as GHC.TypeLits.Presburger.Compat (HsParsedModule(..))
import GHC.Types.TyThing as GHC.TypeLits.Presburger.Compat (lookupTyCon)
import GHC.Builtin.Types (naturalTy)
#else
import GHC.Plugins as GHC.TypeLits.Presburger.Compat 
  ( HsParsedModule(..),
    lookupTyCon,
    typeNatKind
  )
#endif

import GHC.Plugins as GHC.TypeLits.Presburger.Compat
  ( PackageName (..),isStrLitTy, isNumLitTy,
    nilDataCon, consDataCon,
    Hsc,
    Plugin (..),
    TCvSubst (..),
    TvSubstEnv,
    TyVar,
    defaultPlugin,
    emptyTCvSubst,
    eqType,
    mkTcOcc,
    mkTyConTy,
    mkTyVarTy,
    ppr,
    promotedFalseDataCon,
    promotedTrueDataCon,
    purePlugin,
    splitTyConApp,
    splitTyConApp_maybe,
    text,
    tyConAppTyCon_maybe,
    typeKind,
    unionTCvSubst,
  )
import GHC.Tc.Plugin (lookupOrig)
import GHC.Core.InstEnv as GHC.TypeLits.Presburger.Compat (classInstances)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Tc.Plugin (unsafeTcPluginTcM)
import GHC.Utils.Logger (getLogger)
import Data.Functor ((<&>))
import GHC.Unit.Types as GHC.TypeLits.Presburger.Compat (IsBootInterface(..))
#else
import GHC.Driver.Types as GHC.TypeLits.Presburger.Compat (IsBootInterface(..))
#endif

import GHC.Tc.Plugin as GHC.TypeLits.Presburger.Compat
  ( TcPluginM,
    getInstEnvs,
    newFlexiTyVar,
    getTopEnv,
    lookupOrig,
    newFlexiTyVar,
    newWanted,
    matchFam,
    tcLookupClass,
    tcLookupTyCon,
    tcPluginIO,
    tcPluginTrace,
  )
import GHC.Tc.Types as GHC.TypeLits.Presburger.Compat (TcPlugin (..), TcPluginResult (..))
import GHC.Tc.Types.Constraint as GHC.TypeLits.Presburger.Compat
  ( Ct,
    CtEvidence,
    ctEvPred,
    ctEvidence,
    isWanted,
  )
import GHC.Tc.Types.Evidence as GHC.TypeLits.Presburger.Compat (EvTerm)
import GHC.Tc.Utils.TcType (TcTyVar, TcType)
import GHC.Tc.Utils.TcType as GHC.TypeLits.Presburger.Compat (tcTyFamInsts)
import qualified GHC.TcPluginM.Extra as Extra
import GHC.Types.Name.Occurrence as GHC.TypeLits.Presburger.Compat (emptyOccSet, mkInstTyTcOcc)
import GHC.Types.Unique as GHC.TypeLits.Presburger.Compat (getKey, getUnique)
import GHC.Unit.Module as GHC.TypeLits.Presburger.Compat (ModuleName, mkModuleName)
import GHC.Unit.State as GHC.TypeLits.Presburger.Compat (lookupPackageName)
import GHC.Unit.State (initUnits, UnitState (preloadUnits))
import GHC.Unit.Types (UnitId(..), fsToUnit, toUnitId)
import GHC.Utils.Outputable as GHC.TypeLits.Presburger.Compat (showSDocUnsafe)
-- GHC 9 Ends HERE
#else
import Class as GHC.TypeLits.Presburger.Compat (classTyCon, className)
import FastString as GHC.TypeLits.Presburger.Compat (FastString, fsLit, unpackFS)
import GhcPlugins (InScopeSet, Outputable, emptyUFM, InstalledUnitId(..), initPackages, Name)
import GhcPlugins as GHC.TypeLits.Presburger.Compat (PackageName (..), fsToUnitId, lookupPackageName, lookupTyCon, mkTcOcc, mkTyConTy, ppr, promotedFalseDataCon, promotedTrueDataCon, text, tyConAppTyCon_maybe, typeKind, typeNatKind)
import HscTypes as GHC.TypeLits.Presburger.Compat (HscEnv (hsc_dflags))
import Module as GHC.TypeLits.Presburger.Compat (ModuleName, mkModuleName, mkModule)
import Module (Module, UnitId)
import OccName as GHC.TypeLits.Presburger.Compat (emptyOccSet, mkInstTyTcOcc)
import Outputable as GHC.TypeLits.Presburger.Compat (showSDocUnsafe)
import Plugins as GHC.TypeLits.Presburger.Compat (Plugin (..), defaultPlugin)
import PrelNames as GHC.TypeLits.Presburger.Compat (gHC_TYPENATS, dATA_TYPE_EQUALITY)
import qualified PrelNames as Old
import TcEvidence as GHC.TypeLits.Presburger.Compat (EvTerm)
import TcHsType as GHC.TypeLits.Presburger.Compat (tcInferApps)
import TcPluginM as GHC.TypeLits.Presburger.Compat
  ( TcPluginM,
    getTopEnv,
    lookupOrig,
    matchFam,
    newFlexiTyVar,
    newWanted,
    tcLookupClass,
    tcLookupTyCon,
    tcPluginIO,
    tcPluginTrace,
  )
import TcRnMonad as GHC.TypeLits.Presburger.Compat (TcPluginResult (..))
import TcRnTypes as GHC.TypeLits.Presburger.Compat (TcPlugin (..))
import TcType as GHC.TypeLits.Presburger.Compat (tcTyFamInsts)
import TcTypeNats as GHC.TypeLits.Presburger.Compat
import TyCoRep ()
import TyCoRep as GHC.TypeLits.Presburger.Compat (TyLit (NumTyLit), Type (..))
import TyCon as GHC.TypeLits.Presburger.Compat
import Type as GHC.TypeLits.Presburger.Compat (TCvSubst (..), TvSubstEnv, emptyTCvSubst, eqType, mkTyVarTy, splitTyConApp, splitTyConApp_maybe, unionTCvSubst)
import qualified Type as Old
import TysWiredIn as GHC.TypeLits.Presburger.Compat
  ( boolTyCon,
    promotedEQDataCon,
    promotedGTDataCon,
    promotedLTDataCon,
  )
import Unify as Old (tcUnifyTy)
import Unique as GHC.TypeLits.Presburger.Compat (getKey, getUnique)
import Var as GHC.TypeLits.Presburger.Compat (TyVar)
-- Conditional imports for GHC <9
#if MIN_VERSION_ghc(8,4,1)
import TcType (TcTyVar, TcType)
import qualified GHC.TcPluginM.Extra as Extra
import qualified GHC
#else
import Data.Maybe
import TcPluginM (zonkCt)
import TcRnTypes (cc_ev, ctev_pred)
#endif
#if MIN_VERSION_ghc(8,6,0)
import Plugins as GHC.TypeLits.Presburger.Compat (purePlugin)
#endif
#if MIN_VERSION_ghc(8,8,1)
import Name
import TysWiredIn as GHC.TypeLits.Presburger.Compat (eqTyConName) 
import qualified TysWiredIn
#else
import PrelNames as GHC.TypeLits.Presburger.Compat (eqTyConName) 
#endif

#if MIN_VERSION_ghc(8,10,1)
import Predicate as GHC.TypeLits.Presburger.Compat (EqRel (..), Pred(..))
import Predicate as GHC.TypeLits.Presburger.Compat (isEqPred)
import GHC (NoExtField(..))
import qualified Predicate as Old (classifyPredType)
import Predicate as GHC.TypeLits.Presburger.Compat  (mkPrimEqPredRole)
import Constraint as GHC.TypeLits.Presburger.Compat 
    (Ct, ctEvidence, CtEvidence, ctEvPred, isWanted)
#else
import GHC (NoExt(..))
import GhcPlugins as GHC.TypeLits.Presburger.Compat (EqRel (..), PredTree (..))
import GhcPlugins as GHC.TypeLits.Presburger.Compat (isEqPred)
import qualified GhcPlugins as Old (classifyPredType)
import TcRnMonad as GHC.TypeLits.Presburger.Compat (Ct, isWanted)
import Type      as GHC.TypeLits.Presburger.Compat (mkPrimEqPredRole)
import TcRnTypes as GHC.TypeLits.Presburger.Compat (ctEvPred, ctEvidence)
#endif
#endif

#if MIN_VERSION_ghc(8,10,1)
type PredTree = Pred
#endif


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
data TvSubst = TvSubst InScopeSet TvSubstEnv

instance Outputable  TvSubst where
  ppr = ppr . toTCv

emptyTvSubst :: TvSubst
emptyTvSubst = case emptyTCvSubst of
  TCvSubst set tvsenv _ -> TvSubst set tvsenv

toTCv :: TvSubst -> TCvSubst
toTCv (TvSubst set tvenv) = TCvSubst set tvenv emptyUFM

substTy :: TvSubst -> Type -> Type
substTy tvs = Old.substTy (toTCv tvs)

unionTvSubst :: TvSubst -> TvSubst -> TvSubst
unionTvSubst s1 s2 =
  fromTCv $ unionTCvSubst (toTCv s1) (toTCv s2)
fromTCv :: TCvSubst -> TvSubst
fromTCv (TCvSubst set tvsenv _) = TvSubst set tvsenv

promotedBoolTyCon :: TyCon
promotedBoolTyCon = boolTyCon

viewFunTy :: Type -> Maybe (Type, Type)
viewFunTy t@(TyConApp _ [t1, t2])
  | Old.isFunTy t = Just (t1, t2)
viewFunTy _ = Nothing

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 802
#else
pattern FunTy :: Type -> Type -> Type
pattern FunTy t1 t2 <- (viewFunTy -> Just (t1, t2)) where
  FunTy t1 t2 = Old.mkFunTy t1 t2
#endif

tcUnifyTy :: Type -> Type -> Maybe TvSubst
tcUnifyTy t1 t2 = fromTCv <$> Old.tcUnifyTy t1 t2

getEqTyCon :: TcPluginM TyCon
getEqTyCon =
#if MIN_VERSION_ghc(8,8,1)
  return TysWiredIn.eqTyCon
#else
  tcLookupTyCon Old.eqTyConName
#endif

#else
eqType :: Type -> Type -> Bool
eqType = (==)

getEqTyCon :: TcPluginM TyCon
getEqTyCon = return Old.eqTyCon

#endif


getEqWitnessTyCon :: TcPluginM TyCon
getEqWitnessTyCon = do
  md <- lookupModule (mkModuleName "Data.Type.Equality") (fsLit "base")
  tcLookupTyCon =<< lookupOrig md (mkTcOcc ":~:")

decompFunTy :: Type -> [Type]
#if MIN_VERSION_ghc(9,0,0)
decompFunTy (FunTy _ _ t1 t2) = t1 : decompFunTy t2
#else
#if MIN_VERSION_ghc(8,10,1)
decompFunTy (FunTy _ t1 t2) = t1 : decompFunTy t2
#else
decompFunTy (FunTy t1 t2) = t1 : decompFunTy t2
#endif
#endif
decompFunTy t             = [t]

newtype TypeEq = TypeEq { runTypeEq :: Type }

instance Eq TypeEq where
  (==) = geq `on` runTypeEq

instance Ord TypeEq where
  compare = gcompare `on` runTypeEq

isTrivial :: Old.PredType -> Bool
isTrivial ty =
  case classifyPredType ty of
    EqPred _ l r -> l `eqType` r
    _ -> False

normaliseGivens
  :: [Ct] -> TcPluginM [Ct]
normaliseGivens =
#if MIN_VERSION_ghc(8,4,1)
  fmap (return . filter (not . isTrivial . ctEvPred . ctEvidence)) 
  . (++) <$> id <*> Extra.flattenGivens
#else
  mapM zonkCt 
#endif

#if MIN_VERSION_ghc(8,4,1)
type Substitution = [(TcTyVar, TcType)]
#else
type Substitution = TvSubst
#endif

subsCt :: Substitution -> Ct -> Ct
subsCt =
#if MIN_VERSION_ghc(8,4,1)
  Extra.substCt
#else
  \subst ct ->
  ct { cc_ev = (cc_ev ct) {ctev_pred = substTy subst (ctev_pred (cc_ev ct))}
     }
#endif

subsType :: Substitution -> Type -> Type
subsType =
#if MIN_VERSION_ghc(8,4,1)
  Extra.substType
#else
  substTy
#endif

mkSubstitution :: [Ct] -> Substitution
mkSubstitution =
#if MIN_VERSION_ghc(8,4,1)
  map fst . Extra.mkSubst'
#else
  foldr (unionTvSubst . genSubst) emptyTvSubst
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 804
genSubst :: Ct -> TvSubst
genSubst ct = case classifyPredType (ctEvPred . ctEvidence $ ct) of
  EqPred NomEq t u -> fromMaybe emptyTvSubst $ GHC.TypeLits.Presburger.Compat.tcUnifyTy t u
  _                -> emptyTvSubst
#endif


classifyPredType :: Type -> PredTree
classifyPredType ty = case Old.classifyPredType ty of
  e@EqPred{} -> e
  ClassPred cls [_,t1,t2]
    | className cls == eqTyConName
    -> EqPred NomEq t1 t2
  e -> e

#if MIN_VERSION_ghc(9,0,0)
fsToUnitId :: FastString -> UnitId
fsToUnitId = toUnitId . fsToUnit
#endif

type RawUnitId = FastString
preloadedUnitsM :: TcPluginM [FastString] 
#if MIN_VERSION_ghc(9,2,0)
preloadedUnitsM = do
  logger <- unsafeTcPluginTcM getLogger
  dflags <- hsc_dflags <$> getTopEnv
  packs <- tcPluginIO $ initUnits logger dflags Nothing <&> 
    \(_, us, _, _ ) -> preloadUnits us
  let packNames = map (\(UnitId p) -> p) packs
  tcPluginTrace "pres: packs" $ ppr packNames
  pure packNames
#elif MIN_VERSION_ghc(9,0,0)
preloadedUnitsM = do
  dflags <- hsc_dflags <$> getTopEnv
  packs <- tcPluginIO $ preloadUnits . unitState <$> initUnits dflags
  let packNames = map (\(UnitId p) -> p) packs
  tcPluginTrace "pres: packs" $ ppr packNames
  pure packNames
#else
preloadedUnitsM = do
  dflags <- hsc_dflags <$> getTopEnv
  (_, packs) <- tcPluginIO $ initPackages dflags
  let packNames = map (\(InstalledUnitId p) -> p) packs
  tcPluginTrace "pres: packs" $ ppr packNames
  pure packNames
#endif


#if MIN_VERSION_ghc(9,0,0)
type ModuleUnit = Unit
moduleUnit' :: Module -> ModuleUnit
moduleUnit' = moduleUnit
#else
type ModuleUnit = UnitId
moduleUnit' :: Module -> ModuleUnit
moduleUnit' = GHC.moduleUnitId
#endif

#if !MIN_VERSION_ghc(8,10,1)
type NoExtField = NoExt
#endif

noExtField :: NoExtField
#if MIN_VERSION_ghc(8,10,1)
noExtField = NoExtField
#else
noExtField = NoExt
#endif

#if MIN_VERSION_ghc(9,0,1)
type HsModule' = HsModule
#else
type HsModule' = GHC.HsModule GHC.GhcPs
#endif

#if !MIN_VERSION_ghc(9,0,1)
type IsBootInterface = Bool
pattern NotBoot :: IsBootInterface
pattern NotBoot = False

pattern IsBoot :: IsBootInterface
pattern IsBoot = True

{-# COMPLETE NotBoot, IsBoot #-}

#endif
#if MIN_VERSION_ghc(9,2,0)
typeNatKind :: TcType
typeNatKind = naturalTy
#endif

mtypeNatLeqTyCon :: Maybe TyCon
#if MIN_VERSION_ghc(9,2,0)
mtypeNatLeqTyCon = Nothing
#else
mtypeNatLeqTyCon = Just typeNatLeqTyCon
#endif

lookupTyNatPredLeq :: TcPluginM Name
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatPredLeq = do
  tyOrd <- lookupModule (mkModuleName "Data.Type.Ord") "base"
  lookupOrig tyOrd (mkTcOcc "<=")
#else
lookupTyNatPredLeq = 
  lookupOrig gHC_TYPENATS (mkTcOcc "<=")
#endif

lookupTyNatBoolLeq :: TcPluginM TyCon
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatBoolLeq = do
  tyOrd <- lookupModule (mkModuleName "Data.Type.Ord") "base"
  tcLookupTyCon =<< lookupOrig tyOrd (mkTcOcc "<=?")
#else
lookupTyNatBoolLeq = 
  pure typeNatLeqTyCon
#endif

lookupTyNatPredLt :: TcPluginM (Maybe TyCon)
-- Note:  base library shipepd with 9.2.1 has a wrong implementation;
-- hence we MUST NOT desugar it with <= 9.2.1
#if MIN_VERSION_ghc(9,2,2)
lookupTyNatPredLt = Just <$> do
  tyOrd <- lookupModule (mkModuleName "Data.Type.Ord") "base"
  tcLookupTyCon =<< lookupOrig tyOrd (mkTcOcc "<")
#else
lookupTyNatPredLt = pure Nothing
#endif

lookupTyNatBoolLt :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatBoolLt = Just <$> do
  tyOrd <- lookupModule (mkModuleName "Data.Type.Ord") "base"
  tcLookupTyCon =<< lookupOrig tyOrd (mkTcOcc "<?")
#else
lookupTyNatBoolLt = pure Nothing
#endif

lookupTyNatPredGt :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatPredGt = Just <$> do
  tyOrd <- lookupModule (mkModuleName "Data.Type.Ord") "base"
  tcLookupTyCon =<< lookupOrig tyOrd (mkTcOcc ">")
#else
lookupTyNatPredGt = pure Nothing
#endif

lookupTyNatBoolGt :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatBoolGt = Just <$> do
  tyOrd <- lookupModule (mkModuleName "Data.Type.Ord") "base"
  tcLookupTyCon =<< lookupOrig tyOrd (mkTcOcc ">?")
#else
lookupTyNatBoolGt = pure Nothing
#endif

lookupTyNatPredGeq :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatPredGeq = Just <$> do
  tyOrd <- lookupModule (mkModuleName "Data.Type.Ord") "base"
  tcLookupTyCon =<< lookupOrig tyOrd (mkTcOcc ">=")
#else
lookupTyNatPredGeq = pure Nothing
#endif

lookupTyNatBoolGeq :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatBoolGeq = Just <$> do
  tyOrd <- lookupModule (mkModuleName "Data.Type.Ord") "base"
  tcLookupTyCon =<< lookupOrig tyOrd (mkTcOcc ">=?")
#else
lookupTyNatBoolGeq = pure Nothing
#endif

mOrdCondTyCon :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
mOrdCondTyCon = Just <$> do
  tyOrd <- lookupModule (mkModuleName "Data.Type.Ord") "base"
  tcLookupTyCon =<< lookupOrig tyOrd (mkTcOcc "OrdCond")
#else
mOrdCondTyCon = pure Nothing
#endif

lookupTyGenericCompare :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyGenericCompare = Just <$> do
  tyOrd <- lookupModule (mkModuleName "Data.Type.Ord") "base"
  tcLookupTyCon =<< lookupOrig tyOrd (mkTcOcc "Compare")
#else
lookupTyGenericCompare = pure Nothing
#endif
