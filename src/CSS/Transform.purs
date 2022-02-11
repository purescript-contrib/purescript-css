module CSS.Transform where

import Prelude
import CSS.Common (class Inherit, class Initial, class Unset)
import CSS.Property (class Val, Value, noCommas, value)
import CSS.Size (Angle, Size)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Tuple (Tuple(..))

newtype Transformation = Transformation Value

derive instance eqTransformation :: Eq Transformation
derive instance ordTransformation :: Ord Transformation

instance valTransformation :: Val Transformation where
  value (Transformation v) = v

transform :: Transformation -> CSS
transform = key $ fromString "transform"

transforms :: Array Transformation -> CSS
transforms = key (fromString "transform") <<< noCommas

translate :: forall a b. Size a -> Size b -> Transformation
translate x y = Transformation $ fromString "translate(" <> value [ value x, value y ] <> fromString ")"

translateX :: forall a. Size a -> Transformation
translateX x = Transformation $ fromString "translateX(" <> value x <> fromString ")"

translateY :: forall a. Size a -> Transformation
translateY y = Transformation $ fromString "translateY(" <> value y <> fromString ")"

translateZ :: forall a. Size a -> Transformation
translateZ z = Transformation $ fromString "translateZ(" <> value z <> fromString ")"

translate3d :: forall a b c. Size a -> Size b -> Size c -> Transformation
translate3d x y z = Transformation $ fromString "translate3d(" <> value [ value x, value y, value z ] <> fromString ")"

scale :: Number -> Number -> Transformation
scale x y = Transformation $ fromString "scale(" <> value [ x, y ] <> fromString ")"

scaleX :: Number -> Transformation
scaleX x = Transformation $ fromString "scaleX(" <> value x <> fromString ")"

scaleY :: Number -> Transformation
scaleY y = Transformation $ fromString "scaleY(" <> value y <> fromString ")"

scaleZ :: Number -> Transformation
scaleZ z = Transformation $ fromString "scaleZ(" <> value z <> fromString ")"

scale3d :: Number -> Number -> Number -> Transformation
scale3d x y z = Transformation $ fromString "scale3d(" <> value [ x, y, z ] <> fromString ")"

rotate :: forall a. Angle a -> Transformation
rotate a = Transformation $ fromString "rotate(" <> value a <> fromString ")"

rotateX :: forall a. Angle a -> Transformation
rotateX x = Transformation $ fromString "rotateX(" <> value x <> fromString ")"

rotateY :: forall a. Angle a -> Transformation
rotateY y = Transformation $ fromString "rotateY(" <> value y <> fromString ")"

rotateZ :: forall a. Angle a -> Transformation
rotateZ z = Transformation $ fromString "rotateZ(" <> value z <> fromString ")"

rotate3d :: forall a. Number -> Number -> Number -> Angle a -> Transformation
rotate3d x y z a = Transformation $ fromString "rotate3d(" <> value [ value x, value y, value z, value a ] <> fromString ")"

skew :: Number -> Number -> Transformation
skew x y = Transformation $ fromString "skew(" <> value [ x, y ] <> fromString ")"

skewX :: Number -> Transformation
skewX x = Transformation $ fromString "skewX(" <> value x <> fromString ")"

skewY :: Number -> Transformation
skewY y = Transformation $ fromString "skewY(" <> value y <> fromString ")"

perspective :: Number -> Transformation
perspective p = Transformation $ fromString "perspective(" <> value p <> fromString ")"

matrix :: Number -> Number -> Number -> Number -> Number -> Number -> Transformation
matrix u v w x y z = Transformation $ fromString "matrix3d(" <> value [ u, v, w, x, y, z ] <> fromString ")"

matrix3d
  :: Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Transformation
matrix3d w0 x0 y0 z0 w1 x1 y1 z1 w2 x2 y2 z2 w3 x3 y3 z3 =
  Transformation $
    fromString "matrix3d("
      <> value [ w0, x0, y0, z0, w1, x1, y1, z1, w2, x2, y2, z2, w3, x3, y3, z3 ]
      <> fromString ")"

data TransformOrigin :: Type -> Type
data TransformOrigin a
  = TransformOrigin (TransformOriginOffset a) (TransformOriginOffset a) (Size a)
  | Initial
  | Inherit
  | Unset

data TransformOriginOffset :: Type -> Type
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
  value (TransformOrigin x y z) = value (Tuple (Tuple x y) z)
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
