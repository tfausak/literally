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

module Literally where

#include "MachDeps.h"

import qualified Data.Kind as Kind
import qualified Data.Proxy as Proxy
import qualified Data.Tuple as Tuple
import qualified Data.Word as Word
import qualified GHC.TypeLits as Type
import qualified Numeric.Natural as Natural

literal :: forall t -> (FromType t a, KnownType t a) => a
literal t = fromType (Proxy.Proxy :: Proxy.Proxy t)

type FromType :: forall {k}. k -> Kind.Type -> Kind.Constraint
class FromType t a where
  type KnownType t a :: Kind.Constraint
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

#if WORD_SIZE_IN_BITS == 32
instance FromType (n :: Type.Nat) Word.Word where
  type KnownType n Word.Word = (Type.KnownNat n, (Type.<=) n 4294967295)
  fromType = fromInteger . fromType
#elif WORD_SIZE_IN_BITS == 64
instance FromType (n :: Type.Nat) Word.Word where
  type KnownType n Word.Word = (Type.KnownNat n, (Type.<=) n 18446744073709551615)
  fromType = fromInteger . fromType
#else
#error "Unsupported WORD_SIZE_IN_BITS value"
#endif

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
