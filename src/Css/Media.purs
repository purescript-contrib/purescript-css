module Css.Media where

import Prelude
import Css.Property
import Css.Size
import Css.String
import Css.Stylesheet
import Data.Maybe

screen :: MediaType
screen = MediaType $ fromString "screen"

maxWidth :: Size Abs -> Feature
maxWidth = Feature "max-width" <<< Just <<< value
