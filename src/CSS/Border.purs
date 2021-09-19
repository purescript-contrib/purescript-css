module CSS.Border where

import Prelude

import CSS.Color (Color)
import CSS.Property (class Val, Value)
import CSS.Size (Size, Abs)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Generic (class Generic)
import Data.Tuple (Tuple(..))

newtype Stroke = Stroke Value

derive instance eqStroke :: Eq Stroke
derive instance ordStroke :: Ord Stroke

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
border a b c = key (fromString "border") (Tuple a (Tuple b c))

borderTop :: Stroke -> Size Abs -> Color -> CSS
borderTop a b c = key (fromString "border-top") (Tuple a (Tuple b c))

borderBottom :: Stroke -> Size Abs -> Color -> CSS
borderBottom a b c = key (fromString "border-bottom") (Tuple a (Tuple b c))

borderLeft :: Stroke -> Size Abs -> Color -> CSS
borderLeft a b c = key (fromString "border-left") (Tuple a (Tuple b c))

borderRight :: Stroke -> Size Abs -> Color -> CSS
borderRight a b c = key (fromString "border-right") (Tuple a (Tuple b c))

borderColor :: Color -> CSS
borderColor = key $ fromString "border-color"

borderRadius :: forall a. Size a -> Size a -> Size a -> Size a -> CSS
borderRadius a b c d = key (fromString "border-radius") (Tuple (Tuple a b) (Tuple c d))

borderSpacing :: forall a. Size a -> CSS
borderSpacing = key $ fromString "border-spacing"
