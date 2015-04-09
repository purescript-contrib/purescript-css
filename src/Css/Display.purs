module Css.Display where

import Css.Property
import Css.String
import Css.Stylesheet

newtype Display = Display Value

instance valDisplay :: Val Display where
  value (Display v) = v

block :: Display
block = Display $ fromString "block"

display :: Display -> Css
display = key $ fromString "display"
