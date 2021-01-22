# `ghc-typelits-presburger` and `singletons-presburger` -- GHC Plugin for sloving type-level natural constratins with Presburger Arithmetic solver  [![Build Status](https://travis-ci.org/konn/ghc-typelits-presburger.svg?branch=master)](https://travis-ci.org/konn/ghc-typelits-presburger) [![Hackage](https://img.shields.io/hackage/v/ghc-typelits-presburger.svg)](https://hackage.haskell.org/package/ghc-typelits-presburger)

The packages in this monorepo augments type-level naturals in GHC with Presburger Arithmetic solver.
Roughly speaking, it automatically solves constraints expressed by addition, constant-factor multiplicatiojn, and (in)equalities at compile time.

Since 0.3.0.0, integration with [`singletons`][singletons] package is separated to another plugin [`singletons-presburger`][singletons-presburger].
If you need to deal with `singletons` package, please use that instead.

[singletons]: https://hackage.haskell.org/package/singletons
[singletons-presburger]: https://hackage.haskell.org/package/singletons

## Usage
Add this package to your build-depends and add the following pragma on top of your program.

```haskell
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}
```

Since 0.5, one don't need to add `equational-reasoning` to dependency; but if you want to solve constraints involving symbols defined in `equational-reaoning`, such as `IsTrue` and `Empty`, one must explicitly depend on it, otherwise the solver can't handle constructor information.

## Note
This package includes the modified version of code from [`presburger` package](https://hackage.haskell.org/package/presburger) by yav, because yav's original package doesn't compile with GHC 8.4.
