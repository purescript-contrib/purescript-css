module CSS.Time where

import Prelude

import Data.Generic (class Generic)

import CSS.Property (class Val, Value, value)
import CSS.String (fromString)

newtype Time = Time Value

derive instance eqTime :: Eq Time
derive instance ordTime:: Ord Time
derive instance genericTime :: Generic Time

instance valTime :: Val Time where
  value (Time v) = v

sec :: Number -> Time
sec i = Time $ value i <> fromString "s"

ms :: Number -> Time
ms i = Time $ value i <> fromString "ms"
