module CSS.Transform where

import Prelude
import CSS.Common (class Inherit, class Initial, class Unset)
import CSS.Property (class Val, Value, noCommas, value)
import CSS.Size (Angle, Abs, Size)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Generic (class Generic)
import Data.Tuple.Nested (tuple3)

newtype Transformation = Transformation Value

derive instance eqTransformation :: Eq Transformation
derive instance ordTransformation:: Ord Transformation
derive instance genericTransformation :: Generic Transformation

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

data TransformOrigin a
  = TransformOrigin (TransformOriginOffset a) (TransformOriginOffset a) (Size a)
  | Initial
  | Inherit
  | Unset

data TransformOriginOffset a
  = OffsetLength (Size a)
  | OffsetTop
  | OffsetBottom
  | OffsetLeft
  | OffsetRight
  | OffsetCenter

instance valTransformOriginOffset :: Val (TransformOriginOffset a) where
  value (OffsetLength s) = value s
  value (OffsetTop) = fromString "top"
  value (OffsetBottom) = fromString "bottom"
  value (OffsetLeft) = fromString "left"
  value (OffsetRight) = fromString "right"
  value (OffsetCenter) = fromString "center"

instance valTransformOrigin :: Val (TransformOrigin a) where
  value (TransformOrigin x y z) = value (tuple3 x y z)
  value (Initial) = fromString "initial"
  value (Inherit) = fromString "inherit"
  value (Unset) = fromString "unset"

instance initialTransformOrigin :: Initial (TransformOrigin a) where
  initial = Initial

instance inheritTransformOrigin :: Inherit (TransformOrigin a) where
  inherit = Inherit

instance unsetTransformOrigin :: Unset (TransformOrigin a) where
  unset = Unset

offset :: forall a. Size a -> TransformOriginOffset a
offset = OffsetLength

offsetTop :: forall a. TransformOriginOffset a
offsetTop = OffsetTop

offsetBottom :: forall a. TransformOriginOffset a
offsetBottom = OffsetBottom

offsetLeft :: forall a. TransformOriginOffset a
offsetLeft = OffsetLeft

offsetRight :: forall a. TransformOriginOffset a
offsetRight = OffsetRight

offsetCenter :: forall a. TransformOriginOffset a
offsetCenter = OffsetCenter

transformOrigin :: forall a. TransformOriginOffset a -> TransformOriginOffset a -> Size a -> CSS
transformOrigin x y z = key (fromString "transform-origin") (TransformOrigin x y z)
