{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module GHC.TypeLits.Presburger.Syntax.Core where

import Data.Kind (Constraint)
import GHC.TypeLits (Nat, Symbol, type (+))

-- | Package, module, and tysyn name.
data TySynSpec' sym = TySyn sym sym sym
  deriving (Read, Show, Eq, Ord)

type TySynSpec = TySynSpec' Symbol

type DemotedTySynSpec = TySynSpec' String

data ArgSpec' sym nat = ArgOf (TySynSpec' sym) [nat]
  deriving (Read, Show, Eq, Ord)

type ArgSpec = ArgSpec' Symbol Nat

type DemotedArgSpec = ArgSpec' String Int

data Operator
  = (:+:)
  | (:-:)
  | (:*:)
  | Division
  | Modulo
  | LeqBool
  | GeqBool
  | LtBool
  | GtBool
  | LeqConstr
  | GeqConstr
  | LtConstr
  | GtConstr
  | EqBool
  | NeqBool
  | ConstrEq
  | Compare
  deriving (Read, Show, Eq, Ord)

type family Arity (v :: Operator) where
  Arity '(:+:) = 2
  Arity '(:-:) = 2
  Arity '(:*:) = 2
  Arity 'Division = 2
  Arity 'Modulo = 2
  Arity 'LeqBool = 2
  Arity 'GeqBool = 2
  Arity 'LtBool = 2
  Arity 'GtBool = 2
  Arity 'LeqConstr = 2
  Arity 'GeqConstr = 2
  Arity 'LtConstr = 2
  Arity 'GtConstr = 2
  Arity 'EqBool = 2
  Arity 'NeqBool = 2
  Arity 'ConstrEq = 2
  Arity 'Compare = 2

type family Length xs where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

type family ArgOfNary n spc :: Constraint where
  ArgOfNary n ( 'ArgOf _ args) =
    n ~ Length args

class
  ArgOfNary (Arity op) argSpec =>
  PresburgerSyntax (op :: Operator) (argSpec :: ArgSpec)
    | argSpec -> op
