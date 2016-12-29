module CSS.Border where

import Prelude

import Data.Generic (class Generic)
import Data.Tuple.Nested (tuple3, tuple4)

import CSS.Color (Color)
import CSS.Property (class Val, Value)
import CSS.Size (Size, Abs)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

newtype Stroke = Stroke Value

derive instance eqStroke :: Eq Stroke
derive instance ordStroke :: Ord Stroke
derive instance genericStroke :: Generic Stroke

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

border :: Stroke -> Size Abs -> Color -> CSS
border a b c = key (fromString "border") $ tuple3 a b c

borderTop :: Stroke -> Size Abs -> Color -> CSS
borderTop a b c = key (fromString "border-top") $ tuple3 a b c

borderBottom :: Stroke -> Size Abs -> Color -> CSS
borderBottom a b c = key (fromString "border-bottom") $ tuple3 a b c

borderLeft :: Stroke -> Size Abs -> Color -> CSS
borderLeft a b c = key (fromString "border-left") $ tuple3 a b c

borderRight :: Stroke -> Size Abs -> Color -> CSS
borderRight a b c = key (fromString "border-right") $ tuple3 a b c

borderColor :: Color -> CSS
borderColor = key $ fromString "border-color"

borderRadius :: forall a. Size a -> Size a -> Size a -> Size a -> CSS
borderRadius a b c d = key (fromString "border-radius") (tuple4 a b c d)
