{-# LANGUAGE FlexibleContexts, LambdaCase, RecordWildCards #-}
module GHC.TypeLits.Presburger.Syntax.Translate where
import GHC.TypeLits.Presburger.Syntax.Base

import           Class                          (classTyCon)
import           Control.Applicative            ((<|>))
import           Control.Arrow                  (second)
import           Control.Monad                  (mzero)
import           Control.Monad.State.Class      (gets, modify)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Control.Monad.Trans.State      (State, runState)
import           Data.Foldable                  (asum)
import qualified Data.IntMap.Strict             as IM
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (catMaybes, fromMaybe)
import           Data.Reflection
import           GHC.TypeLits.Presburger.Compat
import           Type
import           TysWiredIn                     (promotedEQDataCon,
                                                 promotedGTDataCon,
                                                 promotedLTDataCon)

data MyEnv  = MyEnv { emptyClsTyCon       :: TyCon
                    , eqTyCon_            :: TyCon
                    , eqWitCon_           :: TyCon
                    , isTrueCon_          :: TyCon
                    , voidTyCon           :: TyCon
                    , typeLeqBoolTyCon_   :: TyCon
                    , singCompareCon_     :: TyCon
                    , caseNameForSingLeq_ :: TyCon
                    , caseNameForSingGeq_ :: TyCon
                    , caseNameForSingLt_  :: TyCon
                    , caseNameForSingGt_  :: TyCon
                    }

caseNameForSingLeq :: Given MyEnv => TyCon
caseNameForSingLeq = caseNameForSingLeq_ given
caseNameForSingGeq :: Given MyEnv => TyCon
caseNameForSingGeq = caseNameForSingGeq_ given
caseNameForSingLt  :: Given MyEnv => TyCon
caseNameForSingLt = caseNameForSingLt_ given
caseNameForSingGt  :: Given MyEnv => TyCon
caseNameForSingGt = caseNameForSingGt_ given
emptyTyCon :: Given MyEnv => TyCon
emptyTyCon = emptyClsTyCon given
eqTyCon :: Given MyEnv => TyCon
eqTyCon = eqTyCon_ given

eqWitnessTyCon :: Given MyEnv => TyCon
eqWitnessTyCon = eqWitCon_ given

isTrueTyCon :: Given MyEnv => TyCon
isTrueTyCon = isTrueCon_ given

typeLeqBoolTyCon :: Given MyEnv => TyCon
typeLeqBoolTyCon = typeLeqBoolTyCon_ given

singCompareCon :: Given MyEnv => TyCon
singCompareCon = singCompareCon_ given

boolLeqs :: Given MyEnv => [TyCon]
boolLeqs = [typeNatLeqTyCon, typeLeqBoolTyCon]

data ParseEnv = PEnv { uninterpretedTerms :: M.Map TypeEq Int
                     , variableCount      :: Int
                     }
type Machine = MaybeT (State ParseEnv)

parsePropUnifier :: Given MyEnv
                 => Type
                 -> Machine (SimpleProp Int Int, SimpleProp Int Int)
parsePropUnifier ty = do
  EqPred NomEq t1 t2 <- return $ classifyPredType ty
  (,) <$> toPresburgerPred t1 <*> toPresburgerPred t2

parseExpUnifier :: Given MyEnv
                => Type
                -> Machine (SimpleExp Int, SimpleExp Int)
parseExpUnifier ty = do
  EqPred NomEq t1 t2 <- return $ classifyPredType ty
  (,) <$> toPresburgerExp t1 <*> toPresburgerExp t2

toPresburgerPred :: Given MyEnv => Type -> Machine (SimpleProp Int Int)
toPresburgerPred (TyConApp con [t1, t2])
  | con == typeNatLeqTyCon = (:<=) <$> toPresburgerExp t1 <*> toPresburgerExp t2
toPresburgerPred ty
  | Just (con, []) <- splitTyConApp_maybe ty
  , con == promotedTrueDataCon = return SPTrue
  | Just (con, []) <- splitTyConApp_maybe ty
  , con == promotedFalseDataCon = return SPFalse
  | isEqPred ty = toPresburgerPredTree $ classifyPredType ty
  | Just (con, [l, r]) <- splitTyConApp_maybe ty -- l ~ r
  , con == eqTyCon = toPresburgerPredTree $ EqPred NomEq l r
  | Just (con, [_k, l, r]) <- splitTyConApp_maybe ty -- l (:~: {k}) r
  , con == eqWitnessTyCon = toPresburgerPredTree $ EqPred NomEq l r
  | Just (con, [l]) <- splitTyConApp_maybe ty -- Empty l => ...
  , con == emptyTyCon = Not <$> toPresburgerPred l
  | Just (con, [l]) <- splitTyConApp_maybe ty -- IsTrue l =>
  , con == isTrueTyCon = toPresburgerPred l
  | Just (con, [_,_,_,_,cmpTy]) <- splitTyConApp_maybe ty
  , con == caseNameForSingLeq
  , Just (cmp, [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singCompareCon, typeNatCmpTyCon] =
      (:<=) <$> toPresburgerExp l <*> toPresburgerExp r
  | otherwise = mzero

toPresburgerPredTree :: Given MyEnv => PredTree -> Machine (SimpleProp Int Int)
toPresburgerPredTree (EqPred NomEq p false) -- P ~ 'False <=> Not P ~ 'True
  | Just promotedFalseDataCon  == tyConAppTyCon_maybe false =
    Not <$> toPresburgerPredTree (EqPred NomEq p (mkTyConTy promotedTrueDataCon))
toPresburgerPredTree (EqPred NomEq p b)  -- (n :<=? m) ~ 'True
  | Just promotedTrueDataCon  == tyConAppTyCon_maybe b
  , Just (con, [t1, t2]) <- splitTyConApp_maybe p
  , con `elem` boolLeqs = (:<=) <$> toPresburgerExp t1  <*> toPresburgerExp t2
  | Just promotedTrueDataCon  == tyConAppTyCon_maybe b -- Singleton's <=...
  , Just (con, [_,_,_,_,cmpTy]) <- splitTyConApp_maybe p
  , con == caseNameForSingLeq
  , Just (cmp, [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singCompareCon, typeNatCmpTyCon] =
    (:<=) <$> toPresburgerExp l <*> toPresburgerExp r
  | Just promotedTrueDataCon  == tyConAppTyCon_maybe b -- Singleton's <...
  , Just (con, [_,_,_,_,cmpTy]) <- splitTyConApp_maybe p
  , con == caseNameForSingLt
  , Just (cmp, [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singCompareCon, typeNatCmpTyCon] =
    (:<) <$> toPresburgerExp l <*> toPresburgerExp r
  | Just promotedTrueDataCon  == tyConAppTyCon_maybe b -- Singleton's >=...
  , Just (con, [_,_,_,_,cmpTy]) <- splitTyConApp_maybe p
  , con == caseNameForSingGeq
  , Just (cmp, [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singCompareCon, typeNatCmpTyCon] =
    (:<=) <$> toPresburgerExp r <*> toPresburgerExp l
  | Just promotedTrueDataCon  == tyConAppTyCon_maybe b -- Singleton's >=...
  , Just (con, [_,_,_,_,cmpTy]) <- splitTyConApp_maybe p
  , con == caseNameForSingGt
  , Just (cmp, [l, r]) <- splitTyConApp_maybe cmpTy
  , cmp `elem` [singCompareCon, typeNatCmpTyCon] =
    (:<) <$> toPresburgerExp r <*> toPresburgerExp l
toPresburgerPredTree (EqPred NomEq p q)  -- (p :: Bool) ~ (q :: Bool)
    | typeKind p `eqType` mkTyConTy promotedBoolTyCon =
      (:<=>) <$> toPresburgerPred p
             <*> toPresburgerPred q
toPresburgerPredTree (EqPred NomEq n m)  -- (n :: Nat) ~ (m :: Nat)
  | typeKind n `eqType` typeNatKind =
    (:==) <$> toPresburgerExp n
          <*> toPresburgerExp m
toPresburgerPredTree (EqPred _ t1 t2) -- CmpNat a b ~ CmpNat c d
  | Just (con,  [a, b]) <- splitTyConApp_maybe t1
  , Just (con', [c, d]) <- splitTyConApp_maybe t2
  , con `elem` [singCompareCon, typeNatCmpTyCon], con' `elem` [typeNatCmpTyCon, singCompareCon]
  = (:<=>) <$> ((:<) <$> toPresburgerExp a <*> toPresburgerExp b)
           <*> ((:<) <$> toPresburgerExp c <*> toPresburgerExp d)
toPresburgerPredTree (EqPred NomEq t1 t2) -- CmpNat a b ~ x
  | Just (con, [a, b]) <- splitTyConApp_maybe t1
  , con `elem` [typeNatCmpTyCon, singCompareCon]
  , Just cmp <- tyConAppTyCon_maybe t2 =
    let dic = [(promotedLTDataCon, (:<))
              ,(promotedEQDataCon, (:==))
              ,(promotedGTDataCon, flip (:<))
              ]
    in MaybeT (return $ lookup cmp dic)
        <*> toPresburgerExp a
        <*> toPresburgerExp b
toPresburgerPredTree (EqPred NomEq t1 t2) -- x ~ CmpNat a b
  | Just (con, [a, b]) <- splitTyConApp_maybe t2
  , con `elem` [singCompareCon, typeNatCmpTyCon]
  , Just cmp <- tyConAppTyCon_maybe t1 =
    let dic = [(promotedLTDataCon, (:<))
              ,(promotedEQDataCon, (:==))
              ,(promotedGTDataCon, flip (:<))
              ]
    in MaybeT (return $ lookup cmp dic)
        <*> toPresburgerExp a
        <*> toPresburgerExp b
toPresburgerPredTree (ClassPred con [t1, t2]) -- (n :: Nat) <= (m :: Nat)
  | typeNatLeqTyCon == classTyCon con
  , typeKind t1 `eqType` typeNatKind = (:<=) <$> toPresburgerExp t1 <*> toPresburgerExp t2
toPresburgerPredTree _ = mzero

toPresburgerExp ::  Type -> Machine (SimpleExp Int)
toPresburgerExp ty = case ty of
  TyVarTy t          -> return $ EVar $ getKey $ getUnique t
  t@(TyConApp tc ts) -> body tc ts <|> EVar <$> toVar t
  LitTy (NumTyLit n) -> return (K $ fromInteger n)
  LitTy _            -> mzero
  t                  -> EVar <$> toVar t
  where
    body tc ts =
      let step con op
            | tc == con, [tl, tr] <- ts =
              op <$> toPresburgerExp tl <*> toPresburgerExp tr
            | otherwise = mzero
      in case ts of
        [tl, tr] | tc == typeNatMulTyCon ->
          case (simpleExp tl, simpleExp tr) of
            (LitTy (NumTyLit n), LitTy (NumTyLit m)) -> return $ K $ fromInteger $ n * m
            (LitTy (NumTyLit n), x) -> (:*) <$> pure (fromInteger n) <*> toPresburgerExp x
            (x, LitTy (NumTyLit n)) -> (:*) <$> pure (fromInteger n) <*> toPresburgerExp x
            _ -> mzero
        _ ->  asum [ step con op
                    | (con, op) <- [(typeNatAddTyCon, (:+)), (typeNatSubTyCon, (:-))]]

freshVar :: Machine Int
freshVar = do
  i <- gets variableCount
  modify $ \env -> env { variableCount = i + 1 }
  return i

toVar :: Type -> Machine Int
toVar ty = gets (M.lookup (TypeEq ty) . uninterpretedTerms) >>= \case
  Just v -> return v
  Nothing -> do
    v <- freshVar
    modify $ \env ->
      env { uninterpretedTerms = M.insert (TypeEq ty) v $ uninterpretedTerms env }
    return v

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

decodeProps :: MyEnv -> [Type] -> ([SimpleProp u a], IM.IntMap Type)
decodeProps env tps =
  let variableCount = maximum $ 0 : map (succ . getKey . getUnique) (tyCoVarsOfTypesWellScoped tps)
      uninterpretedTerms = M.empty
      build PEnv{uninterpretedTerms = terms} =
        IM.fromList [(v, k) | (TypeEq k, v) <- M.toList terms]
  in give env $ second build $ flip runState PEnv{..} $ do
    expEqs  <- catMaybes <$> mapM (runMaybeT . parseExpUnifier) tps
    propEqs <- catMaybes <$> mapM (runMaybeT . parsePropUnifier) tps
    return undefined
