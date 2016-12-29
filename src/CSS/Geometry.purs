module CSS.Geometry where

import Data.Function (($))
import Data.Tuple.Nested (tuple4)

import CSS.Size (Size)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

width :: forall a. Size a -> CSS
width = key $ fromString "width"

height :: forall a. Size a -> CSS
height = key $ fromString "height"

minWidth :: forall a. Size a -> CSS
minWidth = key $ fromString "min-width"

minHeight :: forall a. Size a -> CSS
minHeight = key $ fromString "min-height"

maxWidth :: forall a. Size a -> CSS
maxWidth = key $ fromString "max-width"

maxHeight :: forall a. Size a -> CSS
maxHeight = key $ fromString "max-height"

top :: forall a. Size a -> CSS
top = key $ fromString "top"

bottom :: forall a. Size a -> CSS
bottom = key $ fromString "bottom"

left :: forall a. Size a -> CSS
left = key $ fromString "left"

right :: forall a. Size a -> CSS
right = key $ fromString "right"

padding :: forall a. Size a -> Size a -> Size a -> Size a -> CSS
padding a b c d = key (fromString "padding") $ tuple4 a b c d

paddingTop :: forall a. Size a -> CSS
paddingTop = key $ fromString "padding-top"

paddingBottom :: forall a. Size a -> CSS
paddingBottom = key $ fromString "padding-bottom"

paddingLeft :: forall a. Size a -> CSS
paddingLeft = key $ fromString "padding-left"

paddingRight :: forall a. Size a -> CSS
paddingRight = key $ fromString "padding-right"

margin :: forall a. Size a -> Size a -> Size a -> Size a -> CSS
margin a b c d = key (fromString "margin") $ tuple4 a b c d

marginTop :: forall a. Size a -> CSS
marginTop = key $ fromString "margin-top"

marginBottom :: forall a. Size a -> CSS
marginBottom = key $ fromString "margin-bottom"

marginLeft :: forall a. Size a -> CSS
marginLeft = key $ fromString "margin-left"

marginRight :: forall a. Size a -> CSS
marginRight = key $ fromString "margin-right"

lineHeight :: forall a. Size a -> CSS
lineHeight = key $ fromString "line-height"
