# `singletons-presburger` -- GHC Plugin for sloving type-level natural constratins with Presburger Arithmetic solver, with [singletons] support  ![Haskell CI](https://github.com/konn/ghc-typelits-presburger/workflows/Haskell%20CI/badge.svg) [![Hackage](https://img.shields.io/hackage/v/ghc-typelits-presburger.svg)](https://hackage.haskell.org/package/ghc-typelits-presburger)

This package augments type-level naturals in GHC with Presburger Arithmetic solver.
Roughly speaking, it automatically solves constraints expressed by addition, constant-factor, and (in)equalities at compile time.

This package was a part of [`ghc-typelits-presburger`][ghc-typelits-presburger]; since 0.3.0.0, the integration support with [`singletons`][singletons] package is separated as this package.
You can just use this pakcage alone; this plugin does everything `ghc-typelits-presburger` does.

[singletons]: https://hackage.haskell.org/package/singletons
[singletons-presburger]: https://hackage.haskell.org/package/singletons
[ghc-typelits-presburger]: https://hackage.haskell.org/package/ghc-typelits-presburger

## Usage
Add this package to your build-depends and add the following pragma on top of your program.

```haskell
{-# OPTIONS_GHC -fplugin Data.Singletons.TypeNats.Presburger #-}
```

One also has to add `equational-reasoning` package to `build-depends`.
Since this package uses extension mechanism provided by `ghc-typelits-presburger`,
you can use `singletons-presburger` alone, not together with `ghc-typelits-presburger`.
