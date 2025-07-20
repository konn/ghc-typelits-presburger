# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell monorepo containing two GHC plugin packages that provide automated constraint solving for type-level natural numbers using Presburger Arithmetic:

- **ghc-typelits-presburger**: Core plugin for GHC's built-in type-level naturals
- **singletons-presburger**: Extension package with integration for the `singletons` library

The plugins automatically solve type-level constraints involving addition (`+`), constant-factor multiplication (`*`), inequalities (`<=`, `>=`, `<`, `>`), and equalities (`~`).

## Common Commands

### Building
```bash
# Build entire project
cabal v2-build all

# Generate cabal files from package.yaml (run after updating package.yaml)
hpack

# Build with examples enabled
cabal v2-configure --constraint "ghc-typelits-presburger +examples" --constraint "singletons-presburger +examples"
cabal v2-build all
```

### Testing
```bash
# Configure for tests
cabal v2-configure --enable-tests --enable-benchmarks

# Run all tests
cabal v2-test all

# Run specific test suite
cabal v2-test test-typeltis-presburger
```

### Examples
```bash
# Run core examples
cabal v2-configure --constraint "ghc-typelits-presburger +examples"
cabal v2-run simple-arith-core

# Run singletons examples  
cabal v2-configure --constraint "singletons-presburger +examples"
cabal v2-run simple-arith
```

## Architecture

### Package Structure
- Multi-package cabal project with root `cabal.project`
- Each package has `package.yaml` (hpack) that generates `.cabal` files
- CI matrix tests across GHC versions 9.2.8 through 9.10.1

### Core Components

**ghc-typelits-presburger**:
- Main plugin entry: `GHC.TypeLits.Presburger.plugin`
- Core implementation: `GHC.TypeLits.Presburger.Types` (plugin infrastructure, Translation, parsing)
- SAT solver: `Data.Integer.SAT` (modified from `presburger` package)

**singletons-presburger**:
- Main module: `Data.Singletons.TypeNats.Presburger`
- Provides `singletonTranslation` for singletons integration

### Plugin Usage
Add pragma to files using the plugins:
```haskell
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Presburger #-}          -- Core GHC naturals
{-# OPTIONS_GHC -fplugin Data.Singletons.TypeNats.Presburger #-} -- Singletons integration
```

### Key Dependencies
- `ghc-tcplugins-extra`: GHC type checker plugin infrastructure
- `containers`, `mtl`, `transformers`: Standard utilities
- `reflection`: Type-level reflection
- `tasty` + `tasty-discover`: Testing framework with auto-discovery

## Development Notes

- Always run `hpack` after updating `package.yaml` files to regenerate cabal files
- Test suite includes both contradiction detection and successful constraint solving
- CI uses project files in `ci-configs/` for different GHC versions
- HIE configuration available in `hie.yaml` for IDE support

## Deployment and CI

### Freeze Files
- When downloading freeze file under ci-configs, use `wget https://www.stackage.org/{snapshot}/cabal.config -O ci-configs/ghc-{version}.project` command. Please refrain from reading the file contents itself, as they have very large filesize
- When Adding new freeze file to ci-configs, Prepend `import: cabal.project` at the top of freeze file
- When you add new version file under ci-configs, please use full semver such as `ghc-x.y.z.config`, instead of `ghc-x.y.config`