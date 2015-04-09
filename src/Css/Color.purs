module Css.Color where

import Css.Property
import Css.String

data Color = Rgba Number Number Number Number
           | Hsla Number Number Number Number
           | Other Value

instance isStringColor :: IsString Color where
  fromString = Other <<< fromString

instance valColor :: Val Color where
  value (Other v) = v
