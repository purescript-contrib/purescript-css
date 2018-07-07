module CSS.Size where

import Prelude

import CSS.Common (class Auto)
import CSS.Property (class Val, Value, value)
import CSS.String (class IsString, fromString)

newtype Size a = Size Value

derive instance eqSize :: Eq a => Eq (Size a)
derive instance ordSize :: Ord a => Ord (Size a)

instance isStringSize :: IsString (Size a) where
  fromString = Size <<< fromString

instance valSize :: Val (Size a) where
  value (Size v) = v

instance autoSize :: Auto (Size a) where
  auto = fromString "auto"

data Abs
data Rel

-- | Zero size.
nil :: forall a. Size a
nil = Size $ fromString "0"

-- | Unitless size (as recommended for line-height).
unitless ∷ forall a. Number → Size a
unitless = Size <<< value

-- | Size in pixels.
px :: Number -> Size Abs
px i = Size (value i <> fromString "px")

-- | Size in points (1pt = 1/72 of 1in).
pt :: Number -> Size Abs
pt i = Size (value i <> fromString "pt")

-- | Size in em's.
em :: Number -> Size Abs
em i = Size (value i <> fromString "em")

-- | Size in ex'es (x-height of the first avaliable font).
ex :: Number -> Size Abs
ex i = Size (value i <> fromString "ex")

-- | SimpleSize in percents.
pct :: Number -> Size Rel
pct i = Size (value i <> fromString "%")

-- | Size in rem's.
rem :: Number -> Size Rel
rem i = Size (value i <> fromString "rem")

-- | Size in vw's (1vw = 1% of viewport width).
vw :: Number -> Size Rel
vw i = Size (value i <> fromString "vw")

-- | Size in vh's (1vh = 1% of viewport height).
vh :: Number -> Size Rel
vh i = Size (value i <> fromString "vh")

-- | Size in vmin's (the smaller of vw or vh).
vmin :: Number -> Size Rel
vmin i = Size (value i <> fromString "vmin")

-- | Size in vmax's (the larger of vw or vh).
vmax :: Number -> Size Rel
vmax i = Size (value i <> fromString "vmax")

sym :: forall a b. (a -> a -> a -> a -> b) -> a -> b
sym f a = f a a a a

data Deg
data Rad

newtype Angle a = Angle Value

derive instance eqAngle :: Eq a => Eq (Angle a)
derive instance ordAngle :: Ord a => Ord (Angle a)

instance valAngle :: Val (Angle a) where
  value (Angle v) = v

-- | Angle in degrees.
deg :: Number -> Angle Deg
deg i = Angle $ (value i <> fromString "deg")

-- | Angle in radians.
rad :: Number -> Angle Rad
rad i = Angle $ (value i <> fromString "rad")
