-- | Provides a plain Presburger solver plugin for @'GHC.TypeNats.Nat'@.
--
--   For an interface for extension, see
--   "GHC.TypeLits.Presburger.Types".
module GHC.TypeLits.Presburger (plugin) where
import GHC.TypeLits.Presburger.Types
import GhcPlugins

plugin :: Plugin
plugin = pluginWith defaultTranslation
