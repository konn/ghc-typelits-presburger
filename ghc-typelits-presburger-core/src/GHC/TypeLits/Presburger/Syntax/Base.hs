{-# LANGUAGE DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable, DerivingStrategies, StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell, TupleSections                               #-}
module GHC.TypeLits.Presburger.Syntax.Base
  ( SimpleProp(..), SimpleExp(..)
  , varsExp, varsSetExp, varsProp, varsSetProp
  ) where
import           Bound.TH                 (makeBound)
import           Control.Arrow
import           Data.Bifoldable
import           Data.Bifunctor.TH
import qualified Data.DList               as DL
import           Data.Foldable
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Unification.Generic
import           GHC.Generics
import           Numeric.Natural

data SimpleExp v = EVar v
  | K Natural
  | SimpleExp v :+ SimpleExp v
  | SimpleExp v :- SimpleExp v
  | Negate (SimpleExp v)
  | Natural :* SimpleExp v
  deriving (Read, Show, Eq, Ord, Functor, Generic1,
            Generic, Foldable, Traversable)

makeBound ''SimpleExp

deriving anyclass instance HasVar    SimpleExp
deriving anyclass instance Unifiable SimpleExp

data SimpleProp u v = PVar v
  | SPTrue | SPFalse
  | SimpleExp u :== SimpleExp u
  | SimpleExp u :/= SimpleExp u
  | SimpleExp u :<= SimpleExp u
  | SimpleExp u :<  SimpleExp u
  | Not (SimpleProp u v)
  | SimpleProp u v :&& SimpleProp u v
  | SimpleProp u v :|| SimpleProp u v
  | SimpleProp u v :==> SimpleProp u v
  | SimpleProp u v :<=> SimpleProp u v
  deriving (Read, Show, Eq, Ord, Functor,
            Generic, Generic1, Foldable, Traversable
           )

makeBound ''SimpleProp
deriveBifunctor ''SimpleProp
deriveBifoldable ''SimpleProp
deriveBitraversable ''SimpleProp

deriving anyclass instance         HasVar    (SimpleProp u)
deriving anyclass instance Eq u => Unifiable (SimpleProp u)

varsExp :: SimpleExp v -> [v]
varsExp = toList

varsSetExp :: Ord v => SimpleExp v -> Set v
varsSetExp = foldMap Set.singleton

varsProp :: SimpleProp u v -> ([u], [v])
varsProp = bifoldMap ((, mempty) . DL.singleton) ((,) mempty . DL.singleton)
       >>> toList *** toList

varsSetProp :: (Ord u, Ord v) => SimpleProp u v -> (Set u, Set v)
varsSetProp = bifoldMap ((, mempty) . Set.singleton) ((,) mempty . Set.singleton)
