Changelog
==========

## 0.4.0.0
* Fixes constraint solving (fixes #9); this may change the previous (unsound) behaviour, and hence it is breaking change.

## 0.3.0.1
* Supports GHC >= 8.10.

## 0.3.0.0
* Drops support for GHC < 8.4
* Entire overhaul.
* Adds `negated-numbers` option.
* Allows terms which includes uninterpreted terms (still much incomplete).
* Separates `singletons` support as `singletons-presburger` package.
* Provides an interface for extending solver with additional syntax constructs.
  See `GHC.TypeLits.Presburger.Types` module for more detail.
