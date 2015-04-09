module Css.Font where

import Css.Color
import Css.String
import Css.Stylesheet

color :: Color -> Css
color = key $ fromString "color"

