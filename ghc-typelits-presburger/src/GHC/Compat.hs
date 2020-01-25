{-# LANGUAGE CPP, FlexibleInstances, PatternGuards, PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances, ViewPatterns                     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Compat (module GHC.Compat) where
import Data.Function       (on)
import FamInst             as GHC.Compat
import FastString          as GHC.Compat (fsLit)
import GHC.TcPluginM.Extra as GHC.Compat (evByFiat, lookupModule, lookupName,
                                          tracePlugin)
import GhcPlugins          as GHC.Compat (EqRel (..), PredTree (..))
import GhcPlugins          as GHC.Compat (classifyPredType, isEqPred,
                                          lookupTyCon, mkTyConTy)
import GhcPlugins          as GHC.Compat (mkTcOcc, ppr, promotedFalseDataCon)
import GhcPlugins          as GHC.Compat (promotedTrueDataCon, text)
import GhcPlugins          as GHC.Compat (tyConAppTyCon_maybe, typeKind)
import GhcPlugins          as GHC.Compat (typeNatKind)
import Module              as GHC.Compat (ModuleName, mkModuleName)
import OccName             as GHC.Compat (emptyOccSet, mkInstTyTcOcc)
import Plugins             as GHC.Compat (Plugin (..), defaultPlugin)
import TcEvidence          as GHC.Compat (EvTerm)
import TcHsType            as GHC.Compat (tcInferApps)
import TcPluginM           as GHC.Compat (TcPluginM, tcLookupTyCon,
                                          tcPluginTrace)
import TcRnMonad           as GHC.Compat (Ct, TcPluginResult (..), isWanted)
import TcRnTypes           as GHC.Compat (TcPlugin (..), ctEvPred, ctEvidence)
import TcType              as GHC.Compat (tcTyFamInsts)
import TcTypeNats          as GHC.Compat
import TyCon               as GHC.Compat
#if MIN_VERSION_ghc(8,4,1)
import TcType (TcTyVar, TcType)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
import           GhcPlugins (InScopeSet, Outputable, emptyUFM)
import qualified PrelNames  as Old
import           TyCoRep    as GHC.Compat (TyLit (NumTyLit), Type (..))
import           Type       as GHC.Compat (TCvSubst (..), TvSubstEnv,
                                           emptyTCvSubst)
import           Type       as GHC.Compat (eqType, unionTCvSubst)
import qualified Type       as Old
import           TysWiredIn as GHC.Compat (boolTyCon)
import           Unify      as Old (tcUnifyTy)
#else
import Type       as GHC.Compat (TvSubst, emptyTvSubst)
import Type       as GHC.Compat (substTy, unionTvSubst)
import TypeRep    as GHC.Compat (TyLit (NumTyLit), Type (..))
import TysWiredIn as Old (eqTyCon)
import TysWiredIn as GHC.Compat (promotedBoolTyCon)
import Unify      as GHC.Compat (tcUnifyTy)
#endif
import Data.Generics.Twins
import TcPluginM           (lookupOrig)
import TyCoRep             ()
import Type                as GHC.Compat (splitTyConApp_maybe)
import Unique              as GHC.Compat (getKey, getUnique)
#if MIN_VERSION_ghc(8,4,1)
import qualified GHC.TcPluginM.Extra as Extra
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
getEqTyCon = tcLookupTyCon Old.eqTyConName

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
decompFunTy (FunTy t1 t2) = t1 : decompFunTy t2
decompFunTy t             = [t]

newtype TypeEq = TypeEq { runTypeEq :: Type }

instance Eq TypeEq where
  (==) = geq `on` runTypeEq

instance Ord TypeEq where
  compare = gcompare `on` runTypeEq

normaliseGivens
  :: [Ct] -> TcPluginM [Ct]
normaliseGivens gs =
#if MIN_VERSION_ghc(8,4,1)
  return $ gs ++ Extra.flattenGivens gs
#else
  mapM zonkCt givens
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
  fst . unzip . Extra.mkSubst'
#else
  foldr (unionTvSubst . genSubst) emptyTvSubst
#endif
