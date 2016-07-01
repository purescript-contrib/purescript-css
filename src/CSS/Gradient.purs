module CSS.Gradient
  (
  -- * Color ramp type.
    Ramp

  -- * Linear gradients.
  , linearGradient
  , hGradient
  , vGradient

  -- * Radial gradients.
  , Radial
  , circle, ellipse
  , circular, elliptical

  , Extend
  , closestSide, closestCorner, farthestSide, farthestCorner

  , radialGradient

  -- * Repeating gradients.
  , repeatingLinearGradient
  , hRepeatingGradient
  , vRepeatingGradient
  , repeatingRadialGradient

  )
where

import Prelude

import Data.Generic (class Generic)
import Data.Tuple (Tuple(..))

import CSS.Background (class Loc, BackgroundImage, Direction, sideTop, straight, sideLeft)
import CSS.Color (Color)
import CSS.Common (class Other, browsers, other)
import CSS.Property (class Val, Value(..), value)
import CSS.Size (Size, Abs, Rel, pct)
import CSS.String (fromString)

type Ramp = Array (Tuple Color (Size Rel))

-------------------------------------------------------------------------------

linearGradient :: Direction -> Ramp -> BackgroundImage
linearGradient d xs = other $ Value $
  let
    lg =
      fromString "linear-gradient("
        <> value d
        <> fromString ","
        <> ramp xs
        <> fromString ")"
  in
    case lg of
      Value v -> browsers <> v

hGradient :: Color -> Color -> BackgroundImage
hGradient = shortcut (linearGradient (straight sideLeft))

vGradient :: Color -> Color -> BackgroundImage
vGradient = shortcut (linearGradient (straight sideTop ))

-------------------------------------------------------------------------------

repeatingLinearGradient :: Direction -> Ramp -> BackgroundImage
repeatingLinearGradient d xs = other $ Value $
  let
    rlg =
      fromString "repeating-linear-gradient("
        <> value d
        <> fromString ","
        <> ramp xs
        <> fromString ")"
  in
    case rlg of
      Value v -> browsers <> v

hRepeatingGradient :: Color -> Color -> BackgroundImage
hRepeatingGradient = shortcut (repeatingLinearGradient (straight sideLeft))

vRepeatingGradient :: Color -> Color -> BackgroundImage
vRepeatingGradient = shortcut (repeatingLinearGradient (straight sideTop ))

-------------------------------------------------------------------------------

newtype Radial = Radial Value

derive instance eqRadial :: Eq Radial
derive instance ordRadial :: Ord Radial
derive instance genericRadial :: Generic Radial

instance valRadial :: Val Radial where
  value (Radial v) = v

instance otherRadial :: Other Radial where
  other = Radial

circle :: Extend -> Radial
circle ext = Radial (fromString "circle " <> value ext)

ellipse :: Extend -> Radial
ellipse ext = Radial (fromString "ellipse " <> value ext)

circular :: Size Abs -> Radial
circular radius = Radial (value (Tuple radius radius))

elliptical :: forall a. Size a -> Size a -> Radial
elliptical radx rady = Radial (value (Tuple radx rady))

newtype Extend = Extend Value

derive instance eqExtend :: Eq Extend
derive instance ordExtend :: Ord Extend
derive instance genericExtend :: Generic Extend

instance valExtend :: Val Extend where
  value (Extend v) = v

instance otherExtend :: Other Extend where
  other = Extend

closestSide :: Extend
closestSide = Extend $ fromString "closest-side"

closestCorner :: Extend
closestCorner = Extend $ fromString "closest-corner"

farthestSide :: Extend
farthestSide = Extend $ fromString "farthest-side"

farthestCorner :: Extend
farthestCorner = Extend $ fromString "farthest-corner"

-------------------------------------------------------------------------------

radialGradient :: forall l. Loc l => l -> Radial -> Ramp -> BackgroundImage
radialGradient d r xs = other $ Value $
  let
    rg =
      fromString "radial-gradient("
        <> value [value d, value r, ramp xs]
        <> fromString ")"
  in
    case rg of
      Value v -> browsers <> v

repeatingRadialGradient
  :: forall l. Loc l => l -> Radial -> Ramp -> BackgroundImage
repeatingRadialGradient d r xs = other $ Value $
  let
    rrg =
      fromString "repeating-radial-gradient("
        <> value [value d, value r, ramp xs]
        <> fromString ")"
  in
    case rrg of
      Value v -> browsers <> v

-------------------------------------------------------------------------------

ramp :: Ramp -> Value
ramp xs = value (map (\(Tuple a b) -> value (Tuple (value a) (value b))) xs)

shortcut :: (Ramp -> BackgroundImage) -> Color -> Color -> BackgroundImage
shortcut g f t = g [(Tuple f (pct 0.0)), (Tuple t (pct 100.0))]
