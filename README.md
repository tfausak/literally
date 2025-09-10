# Literally

[![CI](https://github.com/tfausak/literally/actions/workflows/ci.yml/badge.svg)](https://github.com/tfausak/literally/actions/workflows/ci.yml)

Literally is a minimal Haskell library that converts type literals into values. It provides type-safe literal conversion functionality using Haskell's advanced type system.

## Overview

The `literally` library allows you to convert compile-time type literals (numbers, strings, characters, etc.) into runtime values in a type-safe manner. This is accomplished through a single `literal` function that leverages type-level programming to ensure correctness at compile time.

## Installation

Add `literally` to your project's dependencies:

```yaml
# In your package.yaml
dependencies:
  - literally
```

Or in your `.cabal` file:

```cabal
build-depends: literally
```

## Usage

Import the module and use the `literal` function with type annotations:

```haskell
{-# LANGUAGE RequiredTypeArguments #-}
import Literally

-- Convert numeric literals
number :: Integer
number = literal 42

naturalNum :: Natural  
naturalNum = literal 123

-- Convert string literals
greeting :: String
greeting = literal "hello"

-- Convert character literals
letter :: Char
letter = literal 'x'

-- Convert boolean literals
truth :: Bool
truth = literal 'True

falsehood :: Bool  
falsehood = literal 'False

-- Convert unit type
unit :: ()
unit = literal ()

-- Works with tuples too
pair :: (Integer, String)
pair = literal (42, "hello")
```

## How It Works

The library uses the `FromType` type class:

- `FromType`: Defines how to convert from a type literal to a value, with an associated `KnownType` constraint

