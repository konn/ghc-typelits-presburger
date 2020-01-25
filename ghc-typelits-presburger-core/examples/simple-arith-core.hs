{-# LANGUAGE CPP, DataKinds, EmptyCase, FlexibleContexts, GADTs, LambdaCase #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, TypeFamilies, TypeInType       #-}
{-# LANGUAGE TypeOperators, UndecidableInstances                            #-}
{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger.Core #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif


module Main where
import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits
import Proof.Propositional (Empty (..), withEmpty)
import Proof.Propositional (IsTrue (Witness))

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 804
type n :* m = n * m
infixl 7 :*
#endif

type n <=! m = IsTrue (n <=? m)
infix 4 <=!

type family Length (as :: [k]) where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

natLen :: (Length xs <= Length ys)
       => proxy xs -> proxy ys -> (Length ys - Length xs) + Length xs :~: Length ys
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

absurdTrueFalse :: ('True :~: 'False) -> a
absurdTrueFalse = \case {}

hoge :: proxy n -> IsTrue (n + 1 <=? n) -> a
hoge _ Witness = absurdTrueFalse Refl

bar :: ((2 :* (n + 1)) ~ ((2 :* n) + 2)) => proxy n -> ()
bar _ = ()

barResult :: ()
barResult = bar (Proxy :: Proxy 2)


trans :: proxy n -> proxy m -> n <=! m -> (n + 1) <=! (m + 1)
trans _ _  Witness = Witness


eqv :: proxy n -> proxy m -> (n <=? m) :~: ((n + 1) <=? (m + 1))
eqv _ _ = Refl


predSucc :: forall proxy n. Empty (n <=! 0) => proxy n -> IsTrue (n + 1 <=? 2 :* n)
predSucc _ = Witness


succLEqLTSucc :: pxy m -> CmpNat 0 (m + 1) :~: 'LT
succLEqLTSucc _ = Refl

succCompare :: pxy n -> pxy m -> CmpNat n m :~: CmpNat (n + 1) (m + 1)
succCompare _ _ = Refl

eqToRefl :: pxy n -> pxy m -> CmpNat n m :~: 'EQ -> n :~: m
eqToRefl _n _m Refl = Refl

main :: IO ()
main = putStrLn "finished"
