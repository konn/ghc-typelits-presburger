{-# LANGUAGE DataKinds, TypeOperators, GADTs, TypeFamilies, ExplicitForAll #-}
{-# OPTIONS_GHC -ddump-tc-trace #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
module Main where
import Data.Type.Equality
import GHC.TypeLits       (type (*), type (+), type (<=), type (<=?))

hoge :: (n + 1 <= n) => ()
hoge = ()

main :: IO ()
main = putStrLn "finished"
