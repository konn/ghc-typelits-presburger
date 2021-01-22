{- | Provides a plain Presburger solver plugin for @'GHC.TypeNats.Nat'@.

   For an interface for extension, see
   "GHC.TypeLits.Presburger.Types".
-}
module GHC.TypeLits.Presburger (plugin) where

import GHC.TypeLits.Presburger.Compat
import GHC.TypeLits.Presburger.Types

plugin :: Plugin
plugin = pluginWith defaultTranslation
