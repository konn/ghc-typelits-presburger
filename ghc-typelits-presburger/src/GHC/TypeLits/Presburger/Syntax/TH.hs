{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module GHC.TypeLits.Presburger.Syntax.TH where

import GHC.TypeLits.Presburger.Syntax.Core
import GHC.TypeNats
import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns ()
import Language.Haskell.TH.Syntax (lift)
import Numeric.Natural (Natural)

symb :: String -> TypeQ
symb = litT . strTyLit

operatorN :: Natural -> Name -> TypeQ
operatorN n op =
  [t| 'ArgOf ( 'TySyn $(symb pkg) $(symb mdl) $(symb opName)) $args|]
  where
    Just pkg = namePackage op
    Just mdl = nameModule op
    opName = nameBase op
    args =
      foldr
        (appT . (promotedConsT `appT`) . litT . numTyLit . fromIntegral)
        promotedNilT
        [0 .. n - 1]
