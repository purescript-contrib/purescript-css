module Css.Property where

import Css.String
import Data.Monoid
import Data.Profunctor.Strong
import Data.Tuple
import Data.Foldable
import qualified Data.Array.NonEmpty as NEL

data Prefixed = Prefixed [Tuple String String]
              | Plain String

instance isStringPrefixed :: IsString Prefixed where
  fromString = Plain

instance semigroupPrefixed :: Semigroup Prefixed where
  (<>) (Plain x) (Plain y) = Plain $ x <> y
  (<>) (Plain x) (Prefixed ys) = Prefixed $ second (x <>) <$> ys
  (<>) (Prefixed xs) (Plain y) = Prefixed $ second (y <>) <$> xs
  -- (<>) (Prefixed xs) (Prefixed ys)

instance monoidPrefixed :: Monoid Prefixed where
  mempty = Plain mempty

newtype Key a = Key Prefixed

instance isStringKey :: IsString (Key a) where
  fromString = Key <<< fromString

cast :: forall a. Key a -> Key Unit
cast (Key k) = Key k

newtype Value = Value Prefixed

instance isStringValue :: IsString Value where
  fromString = Value <<< fromString

instance semigroupValue :: Semigroup Value where
  (<>) (Value a) (Value b) = Value $ a <> b

instance monoidValue :: Monoid Value where
  mempty = Value mempty

class Val a where
  value :: a -> Value

instance valValue :: Val Value where
  value = id

instance valTuple :: (Val a, Val b) => Val (Tuple a b) where
  value (Tuple a b) = value a <> fromString " " <> value b

instance valNumber :: Val Number where
  value = fromString <<< show

instance valList :: (Val a) => Val [a] where
  value = intercalate (fromString ", ") <<< (value <$>)

instance valNonEmpty :: (Val a) => Val (NEL.NonEmpty a) where
  value = value <<< NEL.toArray
