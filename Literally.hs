{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Literally
-- Description : Type-safe conversion of type literals into runtime values
-- Copyright   : (c) Taylor Fausak
-- License     : 0BSD
-- Maintainer  : Taylor Fausak
--
-- Literally is a minimal Haskell library that converts type literals into values
-- using type-safe mechanisms. It provides a single 'literal' function that
-- leverages Haskell's type system to convert compile-time literals into runtime
-- values with compile-time correctness guarantees.
--
-- == Usage
--
-- Enable the @RequiredTypeArguments@ language extension and import this module:
--
-- @
-- {-# LANGUAGE RequiredTypeArguments #-}
-- import Literally
-- @
--
-- Then use the 'literal' function with type annotations:
--
-- @
-- -- Convert numeric literals
-- number = literal 42 :: Integer
-- word8Val = literal 255 :: Word8  -- Bounds checked at compile time
--
-- -- Convert string and character literals
-- greeting = literal "hello" :: String
-- letter = literal 'x' :: Char
--
-- -- Convert boolean literals
-- truth = literal 'True :: Bool
-- falsehood = literal 'False :: Bool
--
-- -- Convert unit and tuple literals
-- unit = literal () :: ()
-- pair = literal (42, "hello") :: (Integer, String)
-- @
--
-- == Type Safety
--
-- The library provides compile-time guarantees:
--
-- * __Bounds checking__: Word types automatically verify values fit within bounds
-- * __Type correctness__: Invalid conversions are caught at compile time
-- * __Known literals__: Only literals determinable at compile time are accepted
--
-- == Supported Types
--
-- The library supports conversion for:
--
-- * Numeric types: 'Integer', 'Natural.Natural', 'Word.Word8', 'Word.Word16', 'Word.Word32', 'Word.Word64', 'Word.Word'
-- * Character types: 'Char'
-- * String types: 'String' (via 'Type.Symbol')
-- * Boolean types: 'Bool' (via @'True@ and @'False@)
-- * Unit type: @()@
-- * Tuple types: 'Tuple.Solo', pairs, and larger tuples
module Literally where

#include "MachDeps.h"

import qualified Data.Kind as Kind
import qualified Data.Proxy as Proxy
import qualified Data.Tuple as Tuple
import qualified Data.Word as Word
import qualified GHC.TypeLits as Type
import qualified Numeric.Natural as Natural

-- | Convert a type literal into a runtime value.
--
-- This is the main function of the library. It takes a type literal as its
-- first argument (using RequiredTypeArguments) and converts it to a runtime
-- value of the appropriate type.
--
-- Examples:
--
-- >>> literal 42 :: Integer
-- 42
--
-- >>> literal "hello" :: String
-- "hello"
--
-- >>> literal 'x' :: Char
-- 'x'
--
-- >>> literal 'True :: Bool
-- True
--
-- The function requires that:
--
-- * The type literal @t@ can be converted to the target type @a@ ('FromType' instance)
-- * The type literal is known at compile time ('KnownType' constraint)
literal :: forall t -> (FromType t a, KnownType t a) => a
literal t = fromType (Proxy.Proxy :: Proxy.Proxy t)

-- | Type class for converting type literals to runtime values.
--
-- This class defines how to convert from a type literal @t@ to a runtime
-- value of type @a@. Each instance also specifies what constraints are
-- required for the type literal to be \"known\" at compile time.
type FromType :: forall {k}. k -> Kind.Type -> Kind.Constraint
class FromType t a where
  -- | The constraint that must be satisfied for the type literal to be known.
  type KnownType t a :: Kind.Constraint

  -- | Convert a type literal (represented by a proxy) to a runtime value.
  fromType :: (KnownType t a) => proxy t -> a

instance FromType (n :: Type.Nat) Integer where
  type KnownType n Integer = Type.KnownNat n
  fromType = Type.natVal

instance FromType (n :: Type.Nat) Natural.Natural where
  type KnownType n Natural.Natural = Type.KnownNat n
  fromType = fromInteger . fromType

instance FromType (n :: Type.Nat) Word.Word8 where
  type KnownType n Word.Word8 = (Type.KnownNat n, (Type.<=) n 255)
  fromType = fromInteger . fromType

instance FromType (n :: Type.Nat) Word.Word16 where
  type KnownType n Word.Word16 = (Type.KnownNat n, (Type.<=) n 65535)
  fromType = fromInteger . fromType

instance FromType (n :: Type.Nat) Word.Word32 where
  type KnownType n Word.Word32 = (Type.KnownNat n, (Type.<=) n 4294967295)
  fromType = fromInteger . fromType

instance FromType (n :: Type.Nat) Word.Word64 where
  type KnownType n Word.Word64 = (Type.KnownNat n, (Type.<=) n 18446744073709551615)
  fromType = fromInteger . fromType

{- ORMOLU_DISABLE -}
#if WORD_SIZE_IN_BITS == 32
#define WORD_MAX 4294967295
#elif WORD_SIZE_IN_BITS == 64
#define WORD_MAX 18446744073709551615
#else
#error "Unsupported WORD_SIZE_IN_BITS value"
#endif
{- ORMOLU_ENABLE -}

instance FromType (n :: Type.Nat) Word.Word where
  type KnownType n Word.Word = (Type.KnownNat n, (Type.<=) n WORD_MAX)
  fromType = fromInteger . fromType

instance FromType (c :: Char) Char where
  type KnownType c Char = Type.KnownChar c
  fromType = Type.charVal

instance FromType (s :: Type.Symbol) String where
  type KnownType s String = Type.KnownSymbol s
  fromType = Type.symbolVal

instance FromType 'True Bool where
  type KnownType 'True Bool = ()
  fromType = const True

instance FromType 'False Bool where
  type KnownType 'False Bool = ()
  fromType = const False

instance FromType () () where
  type KnownType () () = ()
  fromType = const ()

instance (FromType t a) => FromType t (Tuple.Solo a) where
  type KnownType t (Tuple.Solo a) = KnownType t a
  fromType = Tuple.MkSolo . fromType

instance (FromType t a, FromType t b) => FromType t (a, b) where
  type KnownType t (a, b) = (KnownType t a, KnownType t b)
  fromType p = (fromType p, fromType p)
