{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
{-# LANGUAGE DataKinds, TypeOperators, GADTs, TypeFamilies, ExplicitForAll, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, CPP, PolyKinds, TypeInType #-}
#if !MIN_VERSION_singletons(2,4,1)
{-# LANGUAGE KindSignatures #-}
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

module Main where
import Data.Type.Equality
import GHC.TypeLits       (Nat, type (<=?), CmpNat)
import Proof.Propositional (Empty(..))
import Proof.Propositional (IsTrue(Witness))
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List

#if !MIN_VERSION_singletons(2,4,1)
import qualified Data.Singletons.Prelude as Sing
type n <= m = (n :: Nat) Sing.:<= (m :: Nat)
infix 4 <=
type n - m = (n :: Nat) Sing.:- (m :: Nat)
infixl 6 -
type n + m = (n :: Nat) Sing.:+ (m :: Nat)
infixl 6 +
type n * m = (n :: Nat) Sing.:* (m :: Nat)
infixl 7 *
#endif

type n <=! m = IsTrue (n <=? m)
infix 4 <=!

natLen :: (Length xs <= Length ys) ~ 'True
       => proxy xs -> proxy ys -> (Length ys - Length xs) + Length xs :~: Length ys
natLen _ _ = Refl

-- natLeqZero' :: ((n <= 0) ~ 'True) => proxy n -> n :~: 0
-- natLeqZero' _ = Refl

plusLeq :: (n <= m) ~ 'True => proxy (n :: Nat) -> proxy m -> ((m - n) + n :~: m)
plusLeq _ _ = Refl

minusLeq :: (n <= m) ~ 'True => proxy (n :: Nat) -> proxy m -> IsTrue ((m - n) + n <= m)
minusLeq _ _ = Witness

(%:<=?) :: Sing n -> Sing m -> Sing (n <=? m)
n %:<=? m = case sCompare n m of
  SLT -> STrue
  SEQ -> STrue
  SGT -> SFalse

hoge :: ((n + 1 <=? n) ~ 'False) => proxy n -> ()
hoge _ = ()

hoge' :: (((n + 1) <= n) ~ 'False) => proxy n -> ()
hoge' _ = ()

bar :: ((2 * (n + 1)) ~ ((2 * n) + 2)) => proxy n -> ()
bar _ = ()

trans :: proxy n -> proxy m -> n <=! m -> (n + 1) <=! (m + 1)
trans _ _  Witness = Witness

eqv :: proxy n -> proxy m -> (n <=? m) :~: ((n + 1) <=? (m + 1))
eqv _ _ = Refl

leqSucc :: proxy n -> proxy m -> IsTrue ((n + 1) <= m) -> CmpNat n m :~: 'LT
leqSucc _ _ Witness = Refl

predSucc :: forall proxy n. Empty (n <=! 0) => proxy n -> IsTrue (n + 1 <=? 2 * n)
predSucc _ = Witness

main :: IO ()
main = putStrLn "finished"

succLEqLTSucc :: Sing m -> Compare 0 (m + 1) :~: 'LT
succLEqLTSucc _ = Refl

succCompare :: Sing (n :: Nat) -> Sing m -> CmpNat n m :~: CmpNat (n + 1) (m + 1)
succCompare _ _ = Refl

eqToRefl :: Sing (n :: Nat) -> Sing (m :: Nat) -> CmpNat n m :~: 'EQ -> n :~: m
eqToRefl _n _m Refl = Refl

leqEquiv :: (n <= m) ~ 'True => Sing n -> Sing m -> IsTrue (n <=? m)
leqEquiv _ _ = Witness
