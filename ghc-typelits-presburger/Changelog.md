# Changelog

## 0.7.2.0

* Supports GHC 9.6
* More robust `equational-reasoning` package detection logic.

## 0.7.1.0

* Proper Support of GHC 9.4 (Now can solve `Assert` properly)

## 0.7.0.0

* Support GHC 9.4
* The plugin can solve constraints involving type-level `Assert`, `Not`, `(&&)`, `(||)`, and/or `If` from new base.

## 0.6.2.0

* Support GHC 9.2.1
* Decoding in Min/Max expression in terms of OrdCond

## 0.6.0.0

* Stop discharging redundant constraints
* Support GHC 9.0.1
* Drop a support for GHC <8.6

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
