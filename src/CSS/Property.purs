module CSS.Property where

import Prelude
import CSS.String (class IsString, fromString)
import Color (Color, cssStringHSLA)
import Data.Foldable (intercalate)
import Data.Generic (class Generic)
import Data.Maybe (fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.NonEmpty (NonEmpty, oneOf)
import Data.Profunctor.Strong (second)
import Data.Tuple (Tuple(..), lookup)

data Prefixed
  = Prefixed (Array (Tuple String String))
  | Plain String

derive instance eqPrefixed :: Eq Prefixed
derive instance ordPrefixed :: Ord Prefixed
derive instance genericPrefixed :: Generic Prefixed

instance isStringPrefixed :: IsString Prefixed where
  fromString = Plain

instance semigroupPrefixed :: Semigroup Prefixed where
  append (Plain x) (Plain y) = Plain $ x <> y
  append (Plain x) (Prefixed ys) = Prefixed $ second (x <> _) <$> ys
  append (Prefixed xs) (Plain y) = Prefixed $ second (y <> _) <$> xs
  append (Prefixed xs) (Prefixed ys) = Prefixed $ xs <> ys

instance monoidPrefixed :: Monoid Prefixed where
  mempty = Plain mempty

plain :: Prefixed -> String
plain (Prefixed xs) = fromMaybe "" $ lookup "" xs
plain (Plain p) = p

-- TODO: Escape
quote :: String -> String
quote s = "\"" <> s <> "\""

newtype Key a = Key Prefixed

derive instance eqKey :: (Eq a) => Eq (Key a)
derive instance ordKey :: (Ord a) => Ord (Key a)
derive instance genericKey :: (Generic a) => Generic (Key a)

instance isStringKey :: IsString (Key a) where
  fromString = Key <<< fromString

cast :: forall a. Key a -> Key Unit
cast (Key k) = Key k

newtype Value = Value Prefixed

derive instance eqValue :: Eq Value
derive instance ordValue :: Ord Value
derive instance genericValue :: Generic Value

instance isStringValue :: IsString Value where
  fromString = Value <<< fromString

instance semigroupValue :: Semigroup Value where
  append (Value a) (Value b) = Value $ a <> b

instance monoidValue :: Monoid Value where
  mempty = Value mempty

class Val a where
  value :: a -> Value

newtype Literal = Literal String

derive instance eqLiteral :: Eq Literal
derive instance ordLiteral :: Ord Literal
derive instance genericLiteral :: Generic Literal

instance valLiteral :: Val Literal where
  value (Literal a) = fromString $ quote a

instance valValue :: Val Value where
  value = id

instance valString :: Val String where
  value = fromString

instance valUnit :: Val Unit where
  value u = fromString ""

-- When `b` is Unit, the rendered value will have an extra
--   space appended to end. Shouldn't hurt. I'd fix if I knew how.
instance valTuple :: (Val a, Val b) => Val (Tuple a b) where
  value (Tuple a b) = value a <> fromString " " <> value b

instance valNumber :: Val Number where
  value = fromString <<< show

instance valList :: (Val a) => Val (Array a) where
  value = intercalate (fromString ", ") <<< (value <$> _)

instance valNonEmpty :: (Val a) => Val (NonEmpty Array a) where
  value = value <<< oneOf

instance valColor :: Val Color where
  value = fromString <<< cssStringHSLA

noCommas :: forall a. (Val a) => Array a -> Value
noCommas = intercalate (fromString " ") <<< (value <$> _)

infixr 9 Tuple as !
