{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -ddump-tc-trace -ddump-to-file #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

module Main where

import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits
import Proof.Propositional (Empty (..), IsTrue (Witness), withEmpty)

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
absurdTrueFalse = \case

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

main :: IO ()
main = putStrLn "finished"

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

data Dict c where
  Dict :: c => Dict c

data NonEq a b where
  NonEq :: (a == b) ~ 'False => NonEq a b

type family (===) a b :: Bool where
  (===) a a = 'True
  (===) _ _ = 'False

not0ToLeq0 :: (KnownNat n, (n <= 3, (n == 0) ~ 'False, (n === 0) ~ 'False), n <= 3) => proxy n -> Dict (4 - n <= 3)
not0ToLeq0 _ = Dict
