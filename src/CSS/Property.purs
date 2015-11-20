module CSS.Property where

import Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(), fromMaybe)
import Data.Monoid (Monoid, mempty)
import Data.NonEmpty (NonEmpty(), oneOf)
import Data.Profunctor.Strong (second)
import Data.Tuple (Tuple(..), lookup)

import CSS.String

data Prefixed = Prefixed (Array (Tuple String String))
              | Plain String

instance isStringPrefixed :: IsString Prefixed where
  fromString = Plain

instance semigroupPrefixed :: Semigroup Prefixed where
  append (Plain x) (Plain y) = Plain $ x <> y
  append (Plain x) (Prefixed ys) = Prefixed $ second (x <>) <$> ys
  append (Prefixed xs) (Plain y) = Prefixed $ second (y <>) <$> xs
  -- (<>) (Prefixed xs) (Prefixed ys)

instance monoidPrefixed :: Monoid Prefixed where
  mempty = Plain mempty

plain :: Prefixed -> String
plain (Prefixed xs) = fromMaybe "" $ lookup "" xs
plain (Plain    p ) = p

-- TODO: Escape
quote :: String -> String
quote s = "\"" <> s <> "\""

newtype Key a = Key Prefixed

instance isStringKey :: IsString (Key a) where
  fromString = Key <<< fromString

cast :: forall a. Key a -> Key Unit
cast (Key k) = Key k

newtype Value = Value Prefixed

instance isStringValue :: IsString Value where
  fromString = Value <<< fromString

instance semigroupValue :: Semigroup Value where
  append (Value a) (Value b) = Value $ a <> b

instance monoidValue :: Monoid Value where
  mempty = Value mempty

class Val a where
  value :: a -> Value

instance valString :: Val String where
  value = fromString

newtype Literal = Literal String

instance valLiteral :: Val Literal where
  value (Literal a) = fromString $ quote a

instance valValue :: Val Value where
  value = id

instance valTuple :: (Val a, Val b) => Val (Tuple a b) where
  value (Tuple a b) = value a <> fromString " " <> value b

instance valNumber :: Val Number where
  value = fromString <<< show

instance valList :: (Val a) => Val (Array a) where
  value = intercalate (fromString ", ") <<< (value <$>)

instance valNonEmpty :: (Val a) => Val (NonEmpty Array a) where
  value = value <<< oneOf

noCommas :: forall a. (Val a) => Array a -> Value
noCommas = intercalate (fromString " ") <<< (value <$>)
