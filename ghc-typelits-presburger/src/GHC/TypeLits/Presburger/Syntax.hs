{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.TypeLits.Presburger.Syntax
  ( PresburgerSyntax(), Operator(..), operatorN, ArgSpec,
    ArgSpec'(..), DemotedArgSpec, TySynSpec, DemotedTySynSpec,
    TySynSpec'(..)
  ) where

import GHC.TypeLits.Presburger.Syntax.Core
import GHC.TypeLits.Presburger.Syntax.TH
import Data.Type.Equality
import GHC.TypeNats

instance PresburgerSyntax '(:+:) $(operatorN 2 ''(+))
instance PresburgerSyntax '(:-:) $(operatorN 2 ''(-))
instance PresburgerSyntax '(:*:) $(operatorN 2 ''(*))
instance PresburgerSyntax 'Division $(operatorN 2 ''Div)
instance PresburgerSyntax 'Modulo $(operatorN 2 ''Mod)
instance PresburgerSyntax 'EqBool $(operatorN 2 ''(==))
instance PresburgerSyntax 'LeqBool $(operatorN 2 ''(<=?))
instance PresburgerSyntax 'LeqConstr $(operatorN 2 ''(<=))
