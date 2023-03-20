{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif

module Main where

import Unsafe.Coerce
import Data.Proxy
import Numeric.Natural
import Data.Type.Equality
import GHC.TypeLits hiding (SNat)
import Data.Void
import Proof.Propositional (Empty (..), IsTrue (Witness), withEmpty)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 902
import qualified Data.Type.Ord as DTO
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 904
import Data.Type.Bool
#endif

main :: IO ()
main = putStrLn "finished"

type n <=! m = IsTrue (n <=? m)

infix 4 <=!

type family Length (as :: [k]) where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

natLen ::
  (Length xs <= Length ys) =>
  proxy xs ->
  proxy ys ->
  (Length ys - Length xs) + Length xs :~: Length ys
natLen _ _ = Refl

natLeqZero' :: (n <= 0) => proxy n -> n :~: 0
natLeqZero' _ = Refl

leqSucc :: proxy n -> proxy m -> IsTrue ((n + 1) <=? m) -> CmpNat n m :~: 'LT
leqSucc _ _ Witness = Refl

leqEquiv :: (n <= m) => p n -> p m -> IsTrue (n <=? m)
leqEquiv _ _ = Witness

plusLeq :: (n <= m) => proxy (n :: Nat) -> proxy m -> ((m - n) + n :~: m)
plusLeq _ _ = Refl

minusLeq :: (n <= m) => proxy (n :: Nat) -> proxy m -> IsTrue ((m - n) + n <=? m)
minusLeq _ _ = Witness

absurdTrueFalse :: ( 'True :~: 'False) -> a
absurdTrueFalse = \case {}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 802
hoge :: proxy n -> IsTrue (n + 1 <=? n) -> a
hoge _ Witness = absurdTrueFalse Refl
#endif

bar :: (2 * (n + 1)) ~ (2 * n + 2) => proxy n -> ()
bar _ = ()

barResult :: ()
barResult = bar (Proxy :: Proxy 2)

trans :: proxy n -> proxy m -> n <=! m -> (n + 1) <=! (m + 1)
trans _ _ Witness = Witness

eqv :: proxy n -> proxy m -> (n <=? m) :~: ((n + 1) <=? (m + 1))
eqv _ _ = Refl

predSucc :: forall proxy n. Empty (n <=! 0) => proxy n -> IsTrue (n + 1 <=? 2 * n)
predSucc _ = Witness

succLEqLTSucc :: pxy m -> CmpNat 0 (m + 1) :~: 'LT
succLEqLTSucc _ = Refl

succCompare :: pxy n -> pxy m -> CmpNat n m :~: CmpNat (n + 1) (m + 1)
succCompare _ _ = Refl

eqToRefl :: pxy n -> pxy m -> CmpNat n m :~: 'EQ -> n :~: m
eqToRefl _n _m Refl = Refl

rangeEql ::
  ((n == 0) ~ 'False) =>
  pxy n ->
  (1 <=? n) :~: 'True
rangeEql _ = Refl

rangeEqlLeq ::
  ((n == 3) ~ 'False, n <= 3) =>
  pxy n ->
  (n <=? 2) :~: 'True
rangeEqlLeq _ = Refl

data NProxy (n :: Nat) = NProxy

-- GHC >= 9.2 only Tests
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 902

ghc92NLeqToGt :: (n DTO.<=? m) ~ 'False 
  => NProxy n -> NProxy m -> (n DTO.>? m) :~: 'True
ghc92NLeqToGt _ _ = Refl

ghc92GtToGeq :: (n DTO.> m)
  => NProxy n -> NProxy m -> (n DTO.>=? m) :~: 'True
ghc92GtToGeq _ _ = Refl

ghc92GeqEquivNLt :: NProxy n -> NProxy m -> (n DTO.>=? m) :~: ((n DTO.<? m) == 'False)
ghc92GeqEquivNLt _ _ = Refl

-- N.B. We can't replace with predicate style with GHC 9.2.1
-- by the bug in base-4.16.0.0
ghc92NLtToGeq :: (n DTO.<? m) ~ 'True
  => NProxy n -> NProxy m -> (n DTO.>=? m) :~: 'False
ghc92NLtToGeq _ _ = Refl

minLeq :: n <= m => NProxy n -> NProxy m -> DTO.Min n m :~: n
minLeq _ _ = Refl

maxLeq :: n <= m => NProxy n -> NProxy m -> DTO.Max n m :~: m
maxLeq _ _ = Refl

mkOrd :: forall n m. (n DTO.< m) => NProxy n -> NProxy m
mkOrd _ = NProxy

maxOrd :: forall n. (0 DTO.< n) => NProxy n
maxOrd = mkOrd (NProxy @(n - 1))
#endif

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 904
leqGeqToEq :: NProxy n -> NProxy m -> IsTrue (n <=? m && m <=? n) -> n :~: m
leqGeqToEq _ _ Witness = Refl

leqTotal :: NProxy n -> NProxy m -> IsTrue (n <=? m || m <=? n)
leqTotal _ _ = Witness

zeroMinimal :: NProxy n -> (n DTO.<? 0) :~: 'False
zeroMinimal _ = Refl

zeroMinimal' :: NProxy n -> IsTrue (Not (n DTO.<? 0))
zeroMinimal' _ = Witness

caseZero :: NProxy n -> IsTrue (If (n DTO.<=? 0) (n == 0) (n DTO.>? 0))
caseZero _ = Witness

succGtZero :: NProxy n -> IsTrue (0 DTO.<? If (n == 0) (n + 1) n)
succGtZero _ = Witness
#endif

succStepBack :: (Succ n <= Succ m) => NProxy n -> NProxy m -> IsTrue (n <=? m)
succStepBack _ _ = Witness

type Succ n = n + 1

data Leq n m where
  ZeroLeq :: SNat m -> Leq 0 m
  SuccLeqSucc :: Leq n m -> Leq (n + 1) (m + 1)

newtype SNat n = SNat Natural

data ZeroOrSucc n where
  IsZero :: ZeroOrSucc 0
  IsSucc ::
    SNat n ->
    ZeroOrSucc (n + 1)

viewNat :: forall n. SNat n -> ZeroOrSucc n
viewNat (SNat n) =
  if n == 0
    then unsafeCoerce IsZero
    else unsafeCoerce (SNat (n - 1))

pattern Zero :: forall n. () => n ~ 0 => SNat n
pattern Zero <- (viewNat -> IsZero)

pattern Succ :: forall n. () => forall n1. n ~ Succ n1 => SNat n1 -> SNat n
pattern Succ n <- (viewNat -> IsSucc n)

{-# COMPLETE Zero, Succ #-}

succLeqZeroAbsurd :: SNat n -> IsTrue (Succ n <=? 0) -> Void
succLeqZeroAbsurd = undefined

boolToPropLeq :: (n <= m) => SNat n -> SNat m -> Leq n m
boolToPropLeq Zero m = ZeroLeq m
boolToPropLeq (Succ n) (Succ m) = SuccLeqSucc $ boolToPropLeq n m
boolToPropLeq (Succ n) Zero = absurd $ succLeqZeroAbsurd n Witness
