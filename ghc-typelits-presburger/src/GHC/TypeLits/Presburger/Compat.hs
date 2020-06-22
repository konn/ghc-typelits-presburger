{-# LANGUAGE CPP, FlexibleInstances, PatternGuards, PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances, ViewPatterns                     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.TypeLits.Presburger.Compat (module GHC.TypeLits.Presburger.Compat) where
import Data.Function       (on)
import FamInst             as GHC.TypeLits.Presburger.Compat
import FastString          as GHC.TypeLits.Presburger.Compat (fsLit)
import Class
import GHC.TcPluginM.Extra as GHC.TypeLits.Presburger.Compat (evByFiat, lookupModule, lookupName,
                                          tracePlugin)
import GhcPlugins          as GHC.TypeLits.Presburger.Compat (lookupTyCon, mkTyConTy)
import GhcPlugins          as GHC.TypeLits.Presburger.Compat (mkTcOcc, ppr, promotedFalseDataCon)
import GhcPlugins          as GHC.TypeLits.Presburger.Compat (promotedTrueDataCon, text)
import GhcPlugins          as GHC.TypeLits.Presburger.Compat (tyConAppTyCon_maybe, typeKind)
import GhcPlugins          as GHC.TypeLits.Presburger.Compat (typeNatKind)
import Module              as GHC.TypeLits.Presburger.Compat (ModuleName, mkModuleName)
import OccName             as GHC.TypeLits.Presburger.Compat (emptyOccSet, mkInstTyTcOcc)
import Plugins             as GHC.TypeLits.Presburger.Compat (Plugin (..), defaultPlugin)
import TcEvidence          as GHC.TypeLits.Presburger.Compat (EvTerm)
import TcHsType            as GHC.TypeLits.Presburger.Compat (tcInferApps)
import TcPluginM           as GHC.TypeLits.Presburger.Compat (TcPluginM, tcLookupTyCon,
                                          tcPluginTrace)
import TcRnMonad           as GHC.TypeLits.Presburger.Compat (TcPluginResult (..))
import TcRnTypes           as GHC.TypeLits.Presburger.Compat (TcPlugin (..))
import TcType              as GHC.TypeLits.Presburger.Compat (tcTyFamInsts)
import TcTypeNats          as GHC.TypeLits.Presburger.Compat
import TyCon               as GHC.TypeLits.Presburger.Compat
#if MIN_VERSION_ghc(8,4,1)
import TcType (TcTyVar, TcType)
#else
import TcRnTypes (cc_ev, ctev_pred)
import Data.Maybe
import TcPluginM (zonkCt)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
import           GhcPlugins (InScopeSet, Outputable, emptyUFM)
import qualified PrelNames  as Old
import           TyCoRep    as GHC.TypeLits.Presburger.Compat (TyLit (NumTyLit), Type (..))
import           Type       as GHC.TypeLits.Presburger.Compat (TCvSubst (..), TvSubstEnv,
                                           emptyTCvSubst)
import           Type       as GHC.TypeLits.Presburger.Compat (eqType, unionTCvSubst)
import qualified Type       as Old
import           TysWiredIn as GHC.TypeLits.Presburger.Compat (boolTyCon)
import           Unify      as Old (tcUnifyTy)
#else
import Type       as GHC.TypeLits.Presburger.Compat (TvSubst, emptyTvSubst)
import Type       as GHC.TypeLits.Presburger.Compat (substTy, unionTvSubst)
import TypeRep    as GHC.TypeLits.Presburger.Compat (TyLit (NumTyLit), Type (..))
import TysWiredIn as Old (eqTyCon)
import TysWiredIn as GHC.TypeLits.Presburger.Compat (promotedBoolTyCon)
import Unify      as GHC.TypeLits.Presburger.Compat (tcUnifyTy)
#endif
import Data.Generics.Twins
import TcPluginM           (lookupOrig)
import TyCoRep             ()
import Type                as GHC.TypeLits.Presburger.Compat (splitTyConApp_maybe)
import Unique              as GHC.TypeLits.Presburger.Compat (getKey, getUnique)
#if MIN_VERSION_ghc(8,4,1)
import qualified GHC.TcPluginM.Extra as Extra
#endif
#if MIN_VERSION_ghc(8,8,1)
import qualified TysWiredIn
#endif
#if MIN_VERSION_ghc(8,8,1)
import TysWiredIn (eqTyConName)
#else
import PrelNames (eqTyConName)
#endif

#if MIN_VERSION_ghc(8,10,1)
import Predicate as GHC.TypeLits.Presburger.Compat (EqRel (..), Pred(..))
import Predicate as GHC.TypeLits.Presburger.Compat (isEqPred)

import qualified Predicate as Old (classifyPredType)
import Predicate as GHC.TypeLits.Presburger.Compat  (mkPrimEqPredRole)
import Constraint as GHC.TypeLits.Presburger.Compat 
    (Ct, ctEvidence, ctEvPred, isWanted)
#else
import GhcPlugins as GHC.TypeLits.Presburger.Compat (EqRel (..), PredTree (..))
import GhcPlugins as GHC.TypeLits.Presburger.Compat (isEqPred)
import qualified GhcPlugins as Old (classifyPredType)
import TcRnMonad as GHC.TypeLits.Presburger.Compat (Ct, isWanted)
import Type      as GHC.TypeLits.Presburger.Compat (mkPrimEqPredRole)
import TcRnTypes as GHC.TypeLits.Presburger.Compat (ctEvPred, ctEvidence)
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
#if MIN_VERSION_ghc(8,10,1)
decompFunTy (FunTy _ t1 t2) = t1 : decompFunTy t2
#else
decompFunTy (FunTy t1 t2) = t1 : decompFunTy t2
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
  fst . unzip . Extra.mkSubst'
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
