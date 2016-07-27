{-# LANGUAGE DataKinds, TypeOperators, GADTs, TypeFamilies, ExplicitForAll, FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
module Main where
import Data.Type.Equality
import GHC.TypeLits       (type (<=), type (*), type (+), type (<=?))
import Proof.Propositional (Empty(..))
import Proof.Propositional (IsTrue(Witness))

type n <=! m = IsTrue (n <=? m)
infix 4 <=!

natLeqZero :: (n <= 0) => proxy n -> n :~: 0
natLeqZero _ = Refl


hoge :: ((n + 1 <=? n) ~ 'False) => ()
hoge = ()

fuga :: ((n + 1 <=? 0) ~ 'False) => ()
fuga = ()

bar :: ((2 * (n + 1)) ~ ((2 * n) + 2)) => ()
bar = ()

trans :: proxy n -> proxy m -> n <=! m -> (n + 1) <=! (m + 1)
trans _ _  Witness = Witness

eqv :: proxy n -> proxy m -> (n <=? m) :~: ((n + 1) <=? (m + 1))
eqv _ _ = Refl

predSucc :: forall proxy n. Empty (n <=! 0) => proxy n -> IsTrue (n + 1 <=? 2 * n)
predSucc _ = Witness

main :: IO ()
main = putStrLn "finished"
