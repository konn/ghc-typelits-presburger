{-# LANGUAGE DataKinds, TypeOperators, GADTs, TypeFamilies, ExplicitForAll #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
module Main where
import Data.Type.Equality
import GHC.TypeLits       (type (*), type (+), type (<=), type (<=?))

-- hoge :: ((n + 1 <=? n) ~ 'False) => ()
-- hoge = ()

-- fuga :: ((n + 1 <=? 0) ~ 'False) => ()
-- fuga = ()

-- bar :: ((2 * (n + 1)) ~ ((2 * n) + 2)) => ()
-- bar = ()

trans :: (n <= m) => proxy n -> proxy m -> ((n + 1) <=? (m + 1)) :~: True
trans _ _  = Refl

main :: IO ()
main = putStrLn "finished"
