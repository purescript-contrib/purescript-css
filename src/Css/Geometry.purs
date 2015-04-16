module Css.Geometry where

import Css.Size
import Css.String
import Css.Stylesheet
import Data.Tuple.Nested

top :: forall a. Size a -> Css
top = key $ fromString "top"

bottom :: forall a. Size a -> Css
bottom = key $ fromString "bottom"

left :: forall a. Size a -> Css
left = key $ fromString "left"

right :: forall a. Size a -> Css
right = key $ fromString "right"

padding :: forall a. Size a -> Size a -> Size a -> Size a -> Css
padding a b c d = key (fromString "padding") $ tuple4 a b c d

paddingTop :: forall a. Size a -> Css
paddingTop = key $ fromString "padding-top"

paddingBottom :: forall a. Size a -> Css
paddingBottom = key $ fromString "padding-bottom"

paddingLeft :: forall a. Size a -> Css
paddingLeft = key $ fromString "padding-left"

paddingRight :: forall a. Size a -> Css
paddingRight = key $ fromString "padding-right"

margin :: forall a. Size a -> Size a -> Size a -> Size a -> Css
margin a b c d = key (fromString "margin") $ tuple4 a b c d

marginTop :: forall a. Size a -> Css
marginTop = key $ fromString "margin-top"

marginBottom :: forall a. Size a -> Css
marginBottom = key $ fromString "margin-bottom"

marginLeft :: forall a. Size a -> Css
marginLeft = key $ fromString "margin-left"

marginRight :: forall a. Size a -> Css
marginRight = key $ fromString "margin-right"
