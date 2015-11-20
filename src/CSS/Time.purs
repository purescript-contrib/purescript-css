module CSS.Time where

import Prelude

import CSS.Property
import CSS.String

newtype Time = Time Value

instance valTime :: Val Time where
  value (Time v) = v

sec :: Number -> Time
sec i = Time $ value i <> fromString "s"

ms :: Number -> Time
ms i = Time $ value i <> fromString "ms"
