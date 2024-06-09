{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
module GHC.TypeLits.Presburger.Flags (GHCVer(..), ghcVer) where
import GHC.Generics (Generic)

data GHCVer 
  = GHC806 | GHC808 | GHC810 
  | GHC900 | GHC902 | GHC904 
  | GHC906 | GHC908 | GHC910
  deriving (Show, Eq, Ord, Generic)

ghcVer :: GHCVer
#if MIN_VERSION_ghc(9,10,1)
ghcVer = GHC910
#elif MIN_VERSION_ghc(9,8,1)
ghcVer = GHC908
#elif MIN_VERSION_ghc(9,6,1)
ghcVer = GHC906
#elif MIN_VERSION_ghc(9,4,1)
ghcVer = GHC904
#elif MIN_VERSION_ghc(9,2,1)
ghcVer = GHC902
#elif MIN_VERSION_ghc(9,0,1)
ghcVer = GHC900
#elif MIN_VERSION_ghc(8,10,1)
ghcVer = GHC810
#elif MIN_VERSION_ghc(8,8,1)
ghcVer = GHC808
#else
ghcVer = GHC806
#endif
