{-# LANGUAGE CPP, DataKinds, FlexibleContexts, GADTs, PolyKinds     #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeApplications #-}
{-# LANGUAGE TypeFamilies, TypeInType, TypeOperators                #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
{-# OPTIONS_GHC -dcore-lint #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

module Main where
import Data.Singletons.Decide
import Data.Singletons.Prelude
import Data.Singletons.Prelude.Enum
import Data.Singletons.Prelude.List
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Type.Equality
import GHC.TypeLits                 (type (<=?), CmpNat, Nat)
import Proof.Propositional          (Empty (..), withEmpty)
import Proof.Propositional          (IsTrue (Witness))

#if !MIN_VERSION_singletons(2,4,0)
import Data.Promotion.Prelude.Num

type l <= m = l :<= m
type l *  m = l :* m
type l +  m = l :+ m
type l -  m = l :- m
infix 4 <=
infixl 6 +, -
infixl 7 *
#endif

type n <=! m = IsTrue (n <=? m)
infix 4 <=!

natLen :: (Length xs <= Length ys) ~ 'True
       => proxy xs -> proxy ys -> (Length ys - Length xs) + Length xs :~: Length ys
natLen _ _ = Refl


-- The following three cases fails >= GHC 8.6.
natLeqZero' :: ((n <= 0) ~ 'True) => proxy n -> n :~: 0
natLeqZero' _ = Refl

leqSucc :: proxy n -> proxy m -> IsTrue ((n + 1) <= m) -> CmpNat n m :~: 'LT
leqSucc _ _ Witness = Refl

leqEquiv :: (n <= m) ~ 'True => Sing n -> Sing m -> IsTrue (n <=? m)
leqEquiv _ _ = Witness

data NatView (n :: Nat) where
  IsZero :: NatView 0
  IsSucc :: Sing n -> NatView (n + 1)

viewNat :: Sing n -> NatView n
viewNat sn =
  case sn %~ (sing :: Sing 0) of
    Proved Refl    -> IsZero
    Disproved _emp -> withEmpty _emp $ IsSucc $ sPred sn

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

predSucc :: forall proxy n. Empty (n <=! 0) => proxy n -> IsTrue (n + 1 <=? 2 * n)
predSucc _ = Witness

succLEqLTSucc :: Sing m -> Compare 0 (m + 1) :~: 'LT
succLEqLTSucc _ = Refl

succCompare :: Sing (n :: Nat) -> Sing m -> CmpNat n m :~: CmpNat (n + 1) (m + 1)
succCompare _ _ = Refl

eqToRefl :: Sing (n :: Nat) -> Sing (m :: Nat) -> CmpNat n m :~: 'EQ -> n :~: m
eqToRefl _n _m Refl = Refl

singletonsOnly [d|
  flipOrdering :: Ordering -> Ordering
  flipOrdering EQ = EQ
  flipOrdering LT = GT
  flipOrdering GT = LT
 |]

flipCompare
  :: forall n m. (KnownNat n, KnownNat m)
  => Sing n -> Sing m -> FlipOrdering (Compare n m) :~: Compare m n
flipCompare n m = $(sCases ''Ordering [|sCompare n m|] [|Refl|])

ltCompare
  :: forall n m. (KnownNat n, KnownNat m, CmpNat n m ~ LT)
  => Sing n -> Sing m -> Compare m n :~: GT
ltCompare _ _ = Refl

main :: IO ()
main = putStrLn "finished"

