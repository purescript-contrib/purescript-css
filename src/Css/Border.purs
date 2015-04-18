module Css.Border where

import Css.Color
import Css.Property
import Css.Size
import Css.String
import Css.Stylesheet
import Data.Tuple.Nested

newtype Stroke = Stroke Value

instance valStroke :: Val Stroke where
  value (Stroke v) = v

solid :: Stroke
solid = Stroke $ fromString "solid"

dotted :: Stroke
dotted = Stroke $ fromString "dotted"

dashed :: Stroke
dashed = Stroke $ fromString "dashed"

double :: Stroke
double = Stroke $ fromString "double"

wavy :: Stroke
wavy = Stroke $ fromString "wavy"

groove :: Stroke
groove = Stroke $ fromString "groove"

ridge :: Stroke
ridge = Stroke $ fromString "ridge"

inset :: Stroke
inset = Stroke $ fromString "inset"

outset :: Stroke
outset = Stroke $ fromString "outset"

border :: Stroke -> Size Abs -> Color -> Css
border a b c = key (fromString "border") $ tuple3 a b c

borderColor :: Color -> Css
borderColor = key $ fromString "border-color"

borderRadius :: forall a. Size a -> Size a -> Size a -> Size a -> Css
borderRadius a b c d = key (fromString "border-radius") (tuple4 a b c d)
