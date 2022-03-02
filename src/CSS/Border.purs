module CSS.Border
  (
    -- * Stroke type, used for border-style and outline-style.
    Stroke(..)
  , solid
  , dotted
  , dashed
  , double
  , wavy
  , groove
  , ridge
  , inset
  , outset

  -- * Border properties.

  , border
  , borderTop
  , borderLeft
  , borderBottom
  , borderRight
  , borderColor

  -- * Outline properties.

  , outline
  , outlineColor
  , outlineStyle
  , outlineWidth
  , outlineOffset

  -- * Border radius.

  , borderRadius

  -- * Collapsing borders model for a table
  , borderSpacing
  ) where

import Prelude

import CSS.Color (Color)
import CSS.Common (class Inherit, class Initial, class Unset)
import CSS.Property (class Val, Value, (!))
import CSS.Size (Size, LengthUnit)
import CSS.String (class IsString, fromString)
import CSS.Stylesheet (CSS, key)
import Data.Tuple (Tuple(..))

newtype Stroke = Stroke Value

derive instance eqStroke :: Eq Stroke
derive instance ordStroke :: Ord Stroke

instance isStringStroke :: IsString Stroke where
  fromString = Stroke <<< fromString

instance valStroke :: Val Stroke where
  value (Stroke v) = v

instance inheritStroke :: Inherit Stroke where
  inherit = fromString "inherit"

instance initialStroke :: Initial Stroke where
  initial = fromString "initial"

instance unsetStroke :: Unset Stroke where
  unset = fromString "unset"

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

border :: Stroke -> Size LengthUnit -> Color -> CSS
border a b c = key (fromString "border") (Tuple a (Tuple b c))

borderTop :: Stroke -> Size LengthUnit -> Color -> CSS
borderTop a b c = key (fromString "border-top") (Tuple a (Tuple b c))

borderBottom :: Stroke -> Size LengthUnit -> Color -> CSS
borderBottom a b c = key (fromString "border-bottom") (Tuple a (Tuple b c))

borderLeft :: Stroke -> Size LengthUnit -> Color -> CSS
borderLeft a b c = key (fromString "border-left") (Tuple a (Tuple b c))

borderRight :: Stroke -> Size LengthUnit -> Color -> CSS
borderRight a b c = key (fromString "border-right") (Tuple a (Tuple b c))

borderColor :: Color -> CSS
borderColor = key $ fromString "border-color"

-------------------------------------------------------------------------------

outline :: Stroke -> Size LengthUnit -> Color -> CSS
outline a b c = key (fromString "outline") (a ! b ! c)

outlineColor :: Color -> CSS
outlineColor = key (fromString "outline-color")

outlineStyle :: Stroke -> CSS
outlineStyle = key (fromString "outline-style")

outlineWidth :: Size LengthUnit -> CSS
outlineWidth = key (fromString "outline-width")

outlineOffset :: Size LengthUnit -> CSS
outlineOffset = key (fromString "outline-offset")

-------------------------------------------------------------------------------

borderRadius :: forall a. Size a -> Size a -> Size a -> Size a -> CSS
borderRadius a b c d = key (fromString "border-radius") (Tuple (Tuple a b) (Tuple c d))

borderSpacing :: forall a. Size a -> CSS
borderSpacing = key $ fromString "border-spacing"
