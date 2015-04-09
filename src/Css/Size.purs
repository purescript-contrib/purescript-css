module Css.Size where

import Css.Property
import Css.String

newtype Size a = Size Value

instance valSize :: Val (Size a) where
  value (Size v) = v

data Abs

px :: Number -> Size Abs
px i = Size (value i <> fromString "px")
