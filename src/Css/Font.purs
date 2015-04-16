module Css.Font where

import Css.Color
import Css.Property
import Css.String
import Css.Stylesheet
import qualified Data.Array.NonEmpty as NEL

color :: Color -> Css
color = key $ fromString "color"

newtype GenericFontFamily = GenericFontFamily Value

instance valGenericFontFamily :: Val GenericFontFamily where
  value (GenericFontFamily v) = v

sansSerif :: GenericFontFamily
sansSerif = GenericFontFamily $ fromString "sans-serif"

fontFamily :: [String] -> NEL.NonEmpty GenericFontFamily -> Css
fontFamily a b = key (fromString "font-family") $ value b
