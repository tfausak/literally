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

The library uses two main type classes:

- `FromType`: Defines how to convert from a type literal to a value
- `KnownType`: Provides the constraint that the type literal is known at compile time

The `literal` function has the signature:
```haskell
literal :: forall t -> (FromType t a, KnownType t a) => a
```

This ensures that:
1. The type literal `t` can be converted to the target type `a`
2. The type literal is known at compile time
3. The conversion is type-safe

## Supported Types

The library supports conversion for:

- **Numeric types**: `Integer`, `Natural`, `Word8`, `Word16`, `Word32`, `Word64`, `Word`
- **Character types**: `Char`
- **String types**: `String` (via `Symbol`)
- **Boolean types**: `Bool` (via `'True` and `'False`)
- **Unit type**: `()`
- **Tuple types**: `Solo a`, `(a, b)` and larger tuples
- **Bounded numeric types**: Automatic bounds checking for `Word` types

## Examples

### Numeric Literals

```haskell
-- Different numeric types
integerVal = literal 42 :: Integer
naturalVal = literal 42 :: Natural
word8Val = literal 255 :: Word8  -- Bounds checked at compile time
word16Val = literal 65535 :: Word16
```

### String and Character Literals

```haskell
-- String from Symbol
message = literal "Hello, World!" :: String

-- Single character
initial = literal 'H' :: Char
```

### Boolean Literals

```haskell
-- Boolean values using promoted constructors
isTrue = literal 'True :: Bool
isFalse = literal 'False :: Bool
```

### Tuple Literals

```haskell
-- Tuples work when the components can be converted
coordinates = literal (10, 20) :: (Integer, Integer)
mixed = literal (42, "answer") :: (Integer, String)
```

## Type Safety

The library provides compile-time guarantees:

- **Bounds checking**: Word types automatically check that values fit within bounds
- **Type correctness**: Invalid conversions are caught at compile time
- **Known literals**: Only literals that can be determined at compile time are accepted

## License

This project is released under the 0BSD license. See LICENSE.txt for details.

## Contributing

This is a minimal library focused on type-safe literal conversion. Contributions should maintain this focused scope while adding value to the core functionality.