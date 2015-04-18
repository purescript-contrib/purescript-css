module Css.Transform where

import Css.Property
import Css.Size
import Css.String
import Css.Stylesheet

newtype Transformation = Transformation Value

instance valTransformation :: Val Transformation where
  value (Transformation v) = v

transform :: Transformation -> Css
transform = key $ fromString "transform"

transforms :: [Transformation] -> Css
transforms = key (fromString "transform") <<< noCommas

translate :: Size Abs -> Size Abs -> Transformation
translate x y = Transformation $ fromString "translate(" <> value [x, y] <> fromString ")"

rotate :: forall a. Angle a -> Transformation
rotate a = Transformation $ fromString "rotate(" <> value a <> fromString ")"
