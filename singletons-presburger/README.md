# `ghc-typelits-presburger` -- GHC Plugin for Augment Type-level naturals with Presburger solver  [![Build Status](https://travis-ci.org/konn/ghc-typelits-presburger.svg?branch=master)](https://travis-ci.org/konn/ghc-typelits-presburger) [![Hackage](https://img.shields.io/hackage/v/ghc-typelits-presburger.svg)](https://hackage.haskell.org/package/ghc-typelits-presburger)

This package augments type-level naturals in GHC with Presburger Arithmetic solver.
Roughly speaking, it automatically solves constraints expressed by addition, constant-factor, and (in)equalities at compile time.

## Usage
Add this package to your build-depends and add the following pragma on top of your program.

```haskell
{-# OPTIONS_GHC -fplugin Data.Singletons.TypeNats.Presburger #-}
```

## Note
This package includes the modified version of code from [`presburger` package](https://hackage.haskell.org/package/presburger) by yav, because yav's original package doesn't compile with GHC 8.4.
Once these are fixed, we drop that code.
