{-# LANGUAGE OverloadedStrings #-}

module GHC.TypeLits.PresburgerSpec where

import Control.Exception (evaluate, try)
import Control.Exception.Base (TypeError (TypeError))
import Control.Monad (void)
import qualified Data.Text as T
import qualified ErrorsNoPlugin as NoPlugin
import qualified ErrorsWithPlugin as Plugin
import Shared
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

test_recursiveContradiction :: TestTree
test_recursiveContradiction =
  testGroup
    "n ~ n + 1 in recursive call should be rejected as type error"
    [ testCase "Without plugin" $ do
        eith <- try $ void (evaluate $ NoPlugin.zipMVec (True :- Nil) (() :- Nil))
        case eith of
          Left (TypeError msg)
            | "Could not deduce: (n GHC.TypeNats.+ 1) ~ n"
                `T.isInfixOf` T.pack msg ->
              pure ()
          _ -> assertFailure $ "TypeError with mismatch expected, but got: " <> show eith
    , testCase "With plugin" $ do
        eith <- try $ void (evaluate $ Plugin.zipMVec (True :- Nil) (() :- Nil))
        case eith of
          Left (TypeError msg)
            | "Could not deduce: (n GHC.TypeNats.+ 1) ~ n"
                `T.isInfixOf` T.pack msg ->
              pure ()
          _ -> assertFailure $ "TypeError with mismatch expected, but got: " <> show eith
    ]

test_nonrecursiveContradiction :: TestTree
test_nonrecursiveContradiction =
  testGroup
    "n ~ n + 1 in non-recursive call should be rejected as type error"
    [ testCase "Without plugin" $ do
        eith <- try $ void (evaluate $ NoPlugin.unSpin (True :- Nil))
        case eith of
          Left (TypeError msg)
            | "Could not deduce: n1 ~ n"
                `T.isInfixOf` T.pack msg ->
              pure ()
          _ -> assertFailure $ "TypeError with mismatch expected, but got: " <> show eith
    , testCase "With plugin" $ do
        eith <- try $ void (evaluate $ Plugin.unSpin (True :- Nil))
        case eith of
          Left (TypeError msg)
            | "Could not deduce: n1 ~ n"
                `T.isInfixOf` T.pack msg ->
              pure ()
          _ -> assertFailure $ "TypeError with mismatch expected, but got: " <> show eith
    ]
