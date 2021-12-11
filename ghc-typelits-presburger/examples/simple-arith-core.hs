{-# LANGUAGE CPP #-}
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
#endif

module Main where

import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits
import Proof.Propositional (Empty (..), IsTrue (Witness), withEmpty)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 902
import qualified Data.Type.Ord as DTO
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

-- GHC >= 9.2 only Tests
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 902
data NProxy (n :: Nat) = NProxy

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
#endif
