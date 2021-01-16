{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Shared (Vec (..)) where

import GHC.TypeNats (Nat, type (+))

data Vec (n :: Nat) a where
  Nil :: Vec 0 a
  (:-) :: a -> Vec n a -> Vec (n + 1) a

infixr 9 :-
