module CSS.Transform where

import Prelude

import CSS.Property
import CSS.Size
import CSS.String
import CSS.Stylesheet

newtype Transformation = Transformation Value

instance valTransformation :: Val Transformation where
  value (Transformation v) = v

transform :: Transformation -> CSS
transform = key $ fromString "transform"

transforms :: Array Transformation -> CSS
transforms = key (fromString "transform") <<< noCommas

translate :: Size Abs -> Size Abs -> Transformation
translate x y = Transformation $ fromString "translate(" <> value [x, y] <> fromString ")"

rotate :: forall a. Angle a -> Transformation
rotate a = Transformation $ fromString "rotate(" <> value a <> fromString ")"
