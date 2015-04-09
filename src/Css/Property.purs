module Css.Property where

import Css.String
import Data.Tuple

data Prefixed = Prefixed [Tuple String String]
              | Plain String

instance isStringPrefixed :: IsString Prefixed where
  fromString = Plain

newtype Key a = Key Prefixed

instance isStringKey :: IsString (Key a) where
  fromString = Key <<< fromString

cast :: forall a. Key a -> Key Unit
cast (Key k) = Key k

newtype Value = Value Prefixed

instance isStringValue :: IsString Value where
  fromString = Value <<< fromString

class Val a where
  value :: a -> Value
