module Css.Time where

import Css.Property
import Css.String

newtype Time = Time Value

instance valTime :: Val Time where
  value (Time v) = v

sec :: Number -> Time
sec i = Time $ value i <> fromString "s"

ms :: Number -> Time
ms i = Time $ value i <> fromString "ms"
