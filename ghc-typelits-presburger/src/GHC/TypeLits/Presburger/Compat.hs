{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.TypeLits.Presburger.Compat (module GHC.TypeLits.Presburger.Compat) where

import Data.Coerce (coerce)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Generics.Twins
import qualified GHC.Types.Unique as Unique (getKey, Unique)
import GHC.Types.Unique as GHC.TypeLits.Presburger.Compat (Unique, getUnique)
#if MIN_VERSION_ghc(9,10,1)
import GHC.Builtin.Names (gHC_INTERNAL_TYPENATS, gHC_INTERNAL_TYPEERROR)
import GHC.Builtin.Names (mkGhcInternalModule)
#else
import GHC.Builtin.Names (gHC_TYPENATS, gHC_TYPEERROR)
#endif
import GHC.Tc.Types.Constraint as GHC.TypeLits.Presburger.Compat (CtLoc (..), initialSubGoalDepth)
import GHC.Tc.Types.Origin as GHC.TypeLits.Presburger.Compat (CtOrigin (..))
import GHC.TcPluginM.Extra as GHC.TypeLits.Presburger.Compat (
  evByFiat,
  lookupModule,
  lookupName,
  tracePlugin,
 )
#if MIN_VERSION_ghc(9,4,1)
import GHC.Tc.Types as GHC.TypeLits.Presburger.Compat (TcPlugin (..), TcPluginSolveResult (..))
import GHC.Builtin.Types as GHC.TypeLits.Presburger.Compat (cTupleTyCon, cTupleDataCon)
import GHC.Tc.Types.Evidence as GHC.TypeLits.Presburger.Compat (evCast)
import GHC.Plugins as GHC.TypeLits.Presburger.Compat (mkUnivCo)
import GHC.Core.TyCo.Rep as GHC.TypeLits.Presburger.Compat (UnivCoProvenance(..))
import GHC.Core.DataCon as GHC.TypeLits.Presburger.Compat (dataConWrapId)
#else
import GHC.Tc.Types as GHC.TypeLits.Presburger.Compat (TcPlugin (..), TcPluginResult (..))
#endif
#if MIN_VERSION_ghc(9,4,1)
import GHC.Builtin.Names as GHC.TypeLits.Presburger.Compat (mkBaseModule)
import GHC.Core.Reduction (reductionReducedType)
#else
import qualified GHC.Builtin.Names as Old
#endif
import GHC.Builtin.Types as GHC.TypeLits.Presburger.Compat (
  boolTyCon,
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
import GHC.Data.FastString as GHC.TypeLits.Presburger.Compat (FastString, fsLit, unpackFS)
import GHC.Hs as GHC.TypeLits.Presburger.Compat (HsModule (..), NoExtField (..))
import GHC.Hs.Extension as GHC.TypeLits.Presburger.Compat (GhcPs)
import GHC.Hs.ImpExp as GHC.TypeLits.Presburger.Compat (ImportDecl (..), ImportDeclQualifiedStyle (..))
import GHC.Unit.Types (Module, UnitId, toUnitId)
import GHC.Unit.Types as GHC.TypeLits.Presburger.Compat (mkModule)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Env.Types as GHC.TypeLits.Presburger.Compat (HscEnv (hsc_dflags))
#else
import GHC.Driver.Types as GHC.TypeLits.Presburger.Compat (HscEnv (hsc_dflags))
import GHC.Driver.Session (unitState, unitDatabases)
#endif
import GHC.Plugins (InScopeSet, Name, Outputable, Unit, emptyUFM, moduleUnit)
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

#if MIN_VERSION_ghc(9,6,1)
import GHC.Plugins as GHC.TypeLits.Presburger.Compat
  ( Subst (..),
    emptySubst,
    unionSubst,
  )
import GHC.Core.TyCo.Compare as GHC.TypeLits.Presburger.Compat
  (eqType)
#else
import GHC.Plugins as GHC.TypeLits.Presburger.Compat
  ( TCvSubst (..),
    emptyTCvSubst,
    eqType,
    unionTCvSubst,
  )
#endif

import GHC.Core.InstEnv as GHC.TypeLits.Presburger.Compat (classInstances)
import GHC.Plugins as GHC.TypeLits.Presburger.Compat (
  GenericUnitInfo (..),
  Hsc,
  PackageName (..),
  Plugin (..),
  TvSubstEnv,
  TyVar,
  UnitDatabase (..),
  consDataCon,
  defaultPlugin,
  elementOfUniqSet,
  isNumLitTy,
  isStrLitTy,
  mkTcOcc,
  mkTyConTy,
  mkTyVarTy,
  mkUniqSet,
  nilDataCon,
  ppr,
  promotedFalseDataCon,
  promotedTrueDataCon,
  purePlugin,
  splitTyConApp,
  splitTyConApp_maybe,
  text,
  tyConAppTyCon_maybe,
  typeKind,
 )
import GHC.Tc.Plugin (lookupOrig)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Tc.Plugin (unsafeTcPluginTcM)
import GHC.Utils.Logger (getLogger)
import GHC.Unit.Types as GHC.TypeLits.Presburger.Compat (IsBootInterface(..))
#else
import GHC.Driver.Types as GHC.TypeLits.Presburger.Compat (IsBootInterface(..))
#endif

import GHC.Tc.Plugin as GHC.TypeLits.Presburger.Compat (
  TcPluginM,
  getInstEnvs,
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
import GHC.Tc.Types as GHC.TypeLits.Presburger.Compat (TcPlugin (..))
import GHC.Tc.Types.Constraint as GHC.TypeLits.Presburger.Compat (
  Ct,
  CtEvidence,
  ctEvPred,
  ctEvidence,
  isWanted,
 )
import GHC.Tc.Types.Evidence as GHC.TypeLits.Presburger.Compat (EvTerm)
import GHC.Tc.Utils.Monad as GHC.TypeLits.Presburger.Compat (getCtLocM, unsafeTcPluginTcM)
import GHC.Tc.Utils.TcType (TcTyVar, TcType)
import GHC.Tc.Utils.TcType as GHC.TypeLits.Presburger.Compat (tcTyFamInsts)
import qualified GHC.TcPluginM.Extra as Extra
import GHC.Types.Name.Occurrence as GHC.TypeLits.Presburger.Compat (emptyOccSet, mkInstTyTcOcc)
import GHC.Unit.Module as GHC.TypeLits.Presburger.Compat (ModuleName, mkModuleName)
import GHC.Unit.State (UnitState (preloadUnits), initUnits)
import GHC.Unit.State as GHC.TypeLits.Presburger.Compat (lookupPackageName)
import GHC.Unit.Types (UnitId (..), fsToUnit, toUnitId)
import GHC.Utils.Outputable as GHC.TypeLits.Presburger.Compat (showSDocUnsafe)

#if !MIN_VERSION_ghc(9,4,1)
type TcPluginSolveResult = TcPluginResult
#endif

#if MIN_VERSION_ghc(9,4,1)
#if MIN_VERSION_ghc(9,10,1)
dATA_TYPE_EQUALITY :: Module
dATA_TYPE_EQUALITY = mkGhcInternalModule "GHC.Internal.Data.Type.Equality"
#else
dATA_TYPE_EQUALITY :: Module
dATA_TYPE_EQUALITY = mkBaseModule "Data.Type.Equality"
#endif
#endif

#if MIN_VERSION_ghc(9,10,1)
gHC_TYPEERROR :: Module
gHC_TYPEERROR = gHC_INTERNAL_TYPEERROR

gHC_TYPENATS :: Module
gHC_TYPENATS =  gHC_INTERNAL_TYPENATS
#endif


type PredTree = Pred

data TvSubst = TvSubst InScopeSet TvSubstEnv

#if MIN_VERSION_ghc(9,6,1)
type TCvSubst = Subst
unionTCvSubst :: TCvSubst -> TCvSubst -> TCvSubst
unionTCvSubst = unionSubst

emptyTCvSubst :: Subst
emptyTCvSubst = emptySubst
#endif

instance Outputable TvSubst where
  ppr = ppr . toTCv

emptyTvSubst :: TvSubst
#if MIN_VERSION_ghc(9,6,1)
emptyTvSubst = case emptyTCvSubst of
  Subst set _ tvsenv _ -> TvSubst set tvsenv
#else
emptyTvSubst = case emptyTCvSubst of
  TCvSubst set tvsenv _ -> TvSubst set tvsenv
#endif

toTCv :: TvSubst -> TCvSubst
#if MIN_VERSION_ghc(9,6,1)
toTCv (TvSubst set tvenv) = Subst set emptyUFM tvenv emptyUFM
#else
toTCv (TvSubst set tvenv) = TCvSubst set tvenv emptyUFM
#endif

substTy :: TvSubst -> Type -> Type
substTy tvs = Old.substTy (toTCv tvs)

unionTvSubst :: TvSubst -> TvSubst -> TvSubst
unionTvSubst s1 s2 =
  fromTCv $ unionTCvSubst (toTCv s1) (toTCv s2)

fromTCv :: TCvSubst -> TvSubst
#if MIN_VERSION_ghc(9,6,1)
fromTCv (Subst set _ tvsenv _) = TvSubst set tvsenv
#else
fromTCv (TCvSubst set tvsenv _) = TvSubst set tvsenv
#endif

promotedBoolTyCon :: TyCon
promotedBoolTyCon = boolTyCon

viewFunTy :: Type -> Maybe (Type, Type)
viewFunTy t@(TyConApp _ [t1, t2])
  | Old.isFunTy t = Just (t1, t2)
viewFunTy _ = Nothing

tcUnifyTy :: Type -> Type -> Maybe TvSubst
tcUnifyTy t1 t2 = fromTCv <$> Old.tcUnifyTy t1 t2

getEqTyCon :: TcPluginM TyCon
getEqTyCon =
  return TysWiredIn.eqTyCon

getEqWitnessTyCon :: TcPluginM TyCon
getEqWitnessTyCon = do
  tcLookupTyCon =<< lookupOrig dATA_TYPE_EQUALITY (mkTcOcc ":~:")

getEqBoolTyCon :: TcPluginM TyCon
getEqBoolTyCon = do
  tcLookupTyCon =<< lookupOrig dATA_TYPE_EQUALITY (mkTcOcc "==")

decompFunTy :: Type -> [Type]
decompFunTy (FunTy _ _ t1 t2) = t1 : decompFunTy t2
decompFunTy t = [t]

newtype TypeEq = TypeEq {runTypeEq :: Type}

instance Eq TypeEq where
  (==) = geq `on` runTypeEq

instance Ord TypeEq where
  compare = gcompare `on` runTypeEq

isTrivial :: Old.PredType -> Bool
isTrivial ty =
  case classifyPredType ty of
    EqPred _ l r -> l `eqType` r
    _ -> False

normaliseGivens ::
  [Ct] -> TcPluginM [Ct]
normaliseGivens =
  fmap (return . filter (not . isTrivial . ctEvPred . ctEvidence))
    . (++)
    <$> id
    <*> Extra.flattenGivens

#if MIN_VERSION_ghc(8,4,1)
type Substitution = [(TcTyVar, TcType)]
#else
type Substitution = TvSubst
#endif

subsCt :: Substitution -> Ct -> Ct
subsCt = Extra.substCt

subsType :: Substitution -> Type -> Type
subsType = Extra.substType

mkSubstitution :: [Ct] -> Substitution
mkSubstitution = map fst . Extra.mkSubst'

classifyPredType :: Type -> PredTree
classifyPredType ty = case Old.classifyPredType ty of
  e@EqPred {} -> e
  ClassPred cls [_, t1, t2]
    | className cls == eqTyConName ->
        EqPred NomEq t1 t2
  e -> e

fsToUnitId :: FastString -> UnitId
fsToUnitId = toUnitId . fsToUnit

loadedPackageNames ::
  [UnitDatabase UnitId] ->
  UnitState ->
  [RawPackageName]
loadedPackageNames unitDb us =
  let preloads = mkUniqSet $ map (\(UnitId p) -> p) $ preloadUnits us
      ents = filter ((`elementOfUniqSet` preloads) . unitIdFS . unitId) $ concatMap unitDatabaseUnits unitDb
   in map (coerce . unitPackageName) ents

type RawPackageName = FastString

preloadedUnitsM :: TcPluginM [RawPackageName]
#if MIN_VERSION_ghc(9,4,0)
preloadedUnitsM = do
  logger <- unsafeTcPluginTcM getLogger
  dflags <- hsc_dflags <$> getTopEnv
  packNames <- tcPluginIO $ initUnits logger dflags Nothing mempty <&> 
    \(unitDb, us, _, _ ) -> loadedPackageNames unitDb us
  tcPluginTrace "pres: packs" $ ppr packNames
  pure $ coerce packNames
#elif MIN_VERSION_ghc(9,2,0)
preloadedUnitsM = do
  logger <- unsafeTcPluginTcM getLogger
  dflags <- hsc_dflags <$> getTopEnv
  packNames <- tcPluginIO $ initUnits logger dflags Nothing <&> 
    \(unitDb, us, _, _ ) -> loadedPackageNames unitDb us
  tcPluginTrace "pres: packs" $ ppr packNames
  pure packNames
#elif MIN_VERSION_ghc(9,0,0)
preloadedUnitsM = do
  dflags <- hsc_dflags <$> getTopEnv
  packNames <- tcPluginIO $ initUnits dflags <&> \dfs' ->
    let st = unitState dfs'
        db = maybe [] id $ unitDatabases dfs'
     in loadedPackageNames db st
  tcPluginTrace "pres: packs" $ ppr packNames
  pure packNames
#else
preloadedUnitsM = do
  dflags <- hsc_dflags <$> getTopEnv
  (dfs', packs) <- tcPluginIO $ initPackages dflags
  let db = listPackageConfigMap dfs'
      loadeds = mkUniqSet $ map (\(InstalledUnitId p) -> p) packs
      packNames = map (coerce . packageName) $
        filter ((`elementOfUniqSet` loadeds) . coerce . unitId) db
  tcPluginTrace "pres: packs" $ ppr packNames
  pure packNames
#endif

type ModuleUnit = Unit

moduleUnit' :: Module -> ModuleUnit
moduleUnit' = moduleUnit

noExtField :: NoExtField
noExtField = NoExtField

type HsModule' = HsModule

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

dATA_TYPE_ORD :: Module
#if MIN_VERSION_ghc(9,10,0)
dATA_TYPE_ORD = mkGhcInternalModule "GHC.Internal.Data.Type.Ord"
#else
dATA_TYPE_ORD = mkBaseModule "Data.Type.Ord"
#endif

lookupTyNatPredLeq :: TcPluginM Name
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatPredLeq = lookupOrig dATA_TYPE_ORD (mkTcOcc "<=")
#else
lookupTyNatPredLeq = 
  lookupOrig gHC_TYPENATS (mkTcOcc "<=")
#endif

lookupTyNatBoolLeq :: TcPluginM TyCon
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatBoolLeq = tcLookupTyCon =<< lookupOrig dATA_TYPE_ORD (mkTcOcc "<=?")
#else
lookupTyNatBoolLeq = 
  pure typeNatLeqTyCon
#endif

lookupAssertTyCon :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_base(4,17,0)
lookupAssertTyCon = 
  fmap Just . tcLookupTyCon =<< lookupOrig gHC_TYPEERROR (mkTcOcc "Assert")
#else
lookupAssertTyCon = pure Nothing
#endif

lookupTyNatPredLt :: TcPluginM (Maybe TyCon)

-- Note:  base library shipepd with 9.2.1 has a wrong implementation;
-- hence we MUST NOT desugar it with <= 9.2.1
#if MIN_VERSION_ghc(9,2,2)
lookupTyNatPredLt = Just <$> do
  tcLookupTyCon =<< lookupOrig dATA_TYPE_ORD (mkTcOcc "<")
#else
lookupTyNatPredLt = pure Nothing
#endif

lookupTyNatBoolLt :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatBoolLt = Just <$> do
  tcLookupTyCon =<< lookupOrig dATA_TYPE_ORD (mkTcOcc "<?")
#else
lookupTyNatBoolLt = pure Nothing
#endif

lookupTyNatPredGt :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatPredGt = Just <$> do
  tcLookupTyCon =<< lookupOrig dATA_TYPE_ORD (mkTcOcc ">")
#else
lookupTyNatPredGt = pure Nothing
#endif

lookupTyNatBoolGt :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatBoolGt = Just <$> do
  tcLookupTyCon =<< lookupOrig dATA_TYPE_ORD (mkTcOcc ">?")
#else
lookupTyNatBoolGt = pure Nothing
#endif

lookupTyNatPredGeq :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatPredGeq = Just <$> do
  tcLookupTyCon =<< lookupOrig dATA_TYPE_ORD (mkTcOcc ">=")
#else
lookupTyNatPredGeq = pure Nothing
#endif

lookupTyNatBoolGeq :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyNatBoolGeq = Just <$> do
  tcLookupTyCon =<< lookupOrig dATA_TYPE_ORD (mkTcOcc ">=?")
#else
lookupTyNatBoolGeq = pure Nothing
#endif

mOrdCondTyCon :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
mOrdCondTyCon = Just <$> do
  tcLookupTyCon =<< lookupOrig dATA_TYPE_ORD (mkTcOcc "OrdCond")
#else
mOrdCondTyCon = pure Nothing
#endif

lookupTyGenericCompare :: TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,2,0)
lookupTyGenericCompare = Just <$> do
  tcLookupTyCon =<< lookupOrig dATA_TYPE_ORD (mkTcOcc "Compare")
#else
lookupTyGenericCompare = pure Nothing
#endif

lookupBool47 :: String -> TcPluginM (Maybe TyCon)
#if MIN_VERSION_ghc(9,10,0)
lookupBool47 nam = Just <$> do
  tcLookupTyCon =<< lookupOrig (mkGhcInternalModule "GHC.Internal.Data.Type.Bool") (mkTcOcc nam)
#elif MIN_VERSION_base(4,17,0)
lookupBool47 nam = Just <$> do
  tcLookupTyCon =<< lookupOrig (mkBaseModule "Data.Type.Bool") (mkTcOcc nam)
#else
lookupBool47 = const $ pure Nothing
#endif

lookupTyNot, lookupTyIf, lookupTyAnd, lookupTyOr :: TcPluginM (Maybe TyCon)
lookupTyNot = lookupBool47 "Not"
lookupTyIf = lookupBool47 "If"
lookupTyAnd = lookupBool47 "&&"
lookupTyOr = lookupBool47 "||"

matchFam' :: TyCon -> [Type] -> TcPluginM (Maybe Type)
#if MIN_VERSION_ghc(9,4,1)
matchFam' con args = fmap reductionReducedType <$> matchFam con args
#else
matchFam' con args = fmap snd <$> matchFam con args 
#endif

getKey :: Unique.Unique -> Int
#if MIN_VERSION_ghc(9,10,1)
getKey = fromIntegral . Unique.getKey
#else
getKey = Unique.getKey
#endif

getVoidTyCon :: TcPluginM TyCon
#if MIN_VERSION_ghc(9,10,1)
getVoidTyCon = tcLookupTyCon =<< lookupOrig (mkGhcInternalModule "GHC.Internal.Base") (mkTcOcc "Void")
#else
getVoidTyCon = tcLookupTyCon =<< lookupOrig (mkBaseModule "Data.Void") (mkTcOcc "Void")
#endif

