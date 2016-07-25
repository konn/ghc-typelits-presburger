{-# LANGUAGE CPP, PatternGuards, PatternSynonyms, ViewPatterns #-}
module GHC.Compat (module GHC.Compat) where
import FastString          as GHC.Compat (fsLit)
import GHC.TcPluginM.Extra as GHC.Compat (evByFiat, lookupModule, lookupName,
                                          tracePlugin)
import GhcPlugins          as GHC.Compat (EqRel (..), PredTree (..))
import GhcPlugins          as GHC.Compat (classifyPredType, isEqPred,
                                          lookupTyCon, mkTyConTy)
import GhcPlugins          as GHC.Compat (mkTcOcc, ppr, promotedFalseDataCon)
import GhcPlugins          as GHC.Compat (promotedTrueDataCon, text)
import GhcPlugins          as GHC.Compat (tyConAppTyCon_maybe, typeKind)
import GhcPlugins          as GHC.Compat (TyCon, typeNatKind)
import Module              as GHC.Compat (ModuleName, mkModuleName)
import Plugins             as GHC.Compat (Plugin (..), defaultPlugin)
import TcEvidence          as GHC.Compat (EvTerm)
import TcPluginM           as GHC.Compat (TcPluginM, tcLookupTyCon,
                                          tcPluginTrace)
import TcRnMonad           as GHC.Compat (Ct, TcPluginResult (..), isWanted)
import TcRnTypes           as GHC.Compat (TcPlugin (..), ctEvPred, ctEvidence)
import TcTypeNats          as GHC.Compat (typeNatAddTyCon, typeNatExpTyCon)
import TcTypeNats          as GHC.Compat (typeNatLeqTyCon, typeNatMulTyCon)
import TcTypeNats          as GHC.Compat (typeNatSubTyCon)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
import           GhcPlugins (InScopeSet, Outputable, emptyUFM)
import qualified PrelNames  as Old
import           TyCoRep    as GHC.Compat (TyLit (NumTyLit), Type (..))
import           Type       as GHC.Compat (TCvSubst (..), TvSubstEnv,
                                           emptyTCvSubst)
import           Type       as GHC.Compat (eqType, unionTCvSubst)
import qualified Type       as Old
import qualified TysPrim    as Old
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
import TcPluginM (lookupOrig)
import Type      as GHC.Compat (isVoidTy, splitTyConApp_maybe)
import Unique    as GHC.Compat (getKey, getUnique)

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

pattern FunTy :: Type -> Type -> Type
pattern FunTy t1 t2 <- (viewFunTy -> Just (t1, t2)) where
  FunTy t1 t2 = Old.mkFunTy t1 t2

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
decompFunTy t = [t]
