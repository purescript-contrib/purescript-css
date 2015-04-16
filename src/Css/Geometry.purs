module Css.Geometry where

import Css.Size
import Css.String
import Css.Stylesheet
import Data.Tuple.Nested

padding :: forall a. Size a -> Size a -> Size a -> Size a -> Css
padding a b c d = key (fromString "padding") $ tuple4 a b c d

margin :: forall a. Size a -> Size a -> Size a -> Size a -> Css
margin a b c d = key (fromString "margin") $ tuple4 a b c d
