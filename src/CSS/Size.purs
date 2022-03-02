module CSS.Size where

import Prelude

import CSS.Common (class Auto, class Inherit, class Initial, class Unset, browsers)
import CSS.Property (class Val, Prefixed(Plain), Value(..), plain, value)
import CSS.String (class IsString, fromString)
import Data.Exists (Exists, mkExists, runExists)

data LengthUnit

data Percentage

data Combination

data Size :: Type -> Type
data Size a
  = BasicSize Value
  | SumSize (Exists Size) (Exists Size)
  | DiffSize (Exists Size) (Exists Size)
  | MultSize Number (Exists Size)
  | DivSize Number (Exists Size)

type role Size nominal

instance isStringSize :: IsString (Size a) where
  fromString = BasicSize <<< fromString

sizeToString :: forall a. Size a -> String
sizeToString (BasicSize (Value x)) = plain x
sizeToString (SumSize a b) = runExists (\a' -> runExists (\b' -> "(" <> sizeToString a' <> " + " <> sizeToString b' <> ")") b) a
sizeToString (DiffSize a b) = runExists (\a' -> runExists (\b' -> "(" <> sizeToString a' <> " - " <> sizeToString b' <> ")") b) a
sizeToString (MultSize a b) = runExists (\b' -> "(" <> show a <> " * " <> sizeToString b' <> ")") b
sizeToString (DivSize a b) = runExists (\b' -> "(" <> sizeToString b' <> " / " <> show a <> ")") b

instance valSize :: Val (Size a) where
  value (BasicSize x) = x
  value x = Value $ browsers <> Plain ("calc" <> sizeToString x)

instance autoSize :: Auto (Size a) where
  auto = fromString "auto"

instance inheritSize :: Inherit (Size a) where
  inherit = fromString "inherit"

instance initialSize :: Initial (Size a) where
  initial = fromString "initial"

instance unsetSize :: Unset (Size a) where
  unset = fromString "unset"

-- | Zero size.
nil :: forall a. Size a
nil = BasicSize $ fromString "0"

-- | Unitless size (as recommended for line-height).
unitless :: forall a. Number -> Size a
unitless = BasicSize <<< value

-- | Size in pixels.
px :: Number -> Size LengthUnit
px i = BasicSize (value i <> fromString "px")

-- | Size in points (1pt = 1/72 of 1in).
pt :: Number -> Size LengthUnit
pt i = BasicSize (value i <> fromString "pt")

-- | Size in em's.
em :: Number -> Size LengthUnit
em i = BasicSize (value i <> fromString "em")

-- | Size in ex'es (x-height of the first avaliable font).
ex :: Number -> Size LengthUnit
ex i = BasicSize (value i <> fromString "ex")

ch :: Number -> Size LengthUnit
ch i = BasicSize (value i <> fromString "ch")

-- | SimpleSize in percents.
pct :: Number -> Size Percentage
pct i = BasicSize (value i <> fromString "%")

-- | Size in rem's.
rem :: Number -> Size LengthUnit
rem i = BasicSize (value i <> fromString "rem")

-- | Size in vw's (1vw = 1% of viewport width).
vw :: Number -> Size LengthUnit
vw i = BasicSize (value i <> fromString "vw")

-- | Size in vh's (1vh = 1% of viewport height).
vh :: Number -> Size LengthUnit
vh i = BasicSize (value i <> fromString "vh")

-- | Size in vmin's (the smaller of vw or vh).
vmin :: Number -> Size LengthUnit
vmin i = BasicSize (value i <> fromString "vmin")

-- | Size in vmax's (the larger of vw or vh).
vmax :: Number -> Size LengthUnit
vmax i = BasicSize (value i <> fromString "vmax")

class SizeCombination :: forall a b c. a -> b -> c -> Constraint
class SizeCombination a b c | a -> c, b -> c

instance SizeCombination Percentage Percentage Percentage
instance SizeCombination LengthUnit LengthUnit LengthUnit
instance SizeCombination Percentage LengthUnit Combination
instance SizeCombination LengthUnit Percentage Combination

infixl 6 calcSum as @+@

calcSum :: forall a b c. SizeCombination a b c => Size a -> Size b -> Size c
calcSum a b = SumSize (mkExists a) (mkExists b)

infixl 6 calcDiff as @-@

calcDiff :: forall a b c. SizeCombination a b c => Size a -> Size b -> Size c
calcDiff a b = DiffSize (mkExists a) (mkExists b)

infixl 7 calcMult as *@

calcMult :: forall a. Number -> Size a -> Size a
calcMult a b = MultSize a $ mkExists b

infixl 7 calcMultFlipped as @*

calcMultFlipped :: forall a. Size a -> Number -> Size a
calcMultFlipped = flip calcMult

infixl 7 calcDiv as @/

calcDiv :: forall a. Size a -> Number -> Size a
calcDiv a b = DivSize b $ mkExists a

sym :: forall a b. (a -> a -> a -> a -> b) -> a -> b
sym f a = f a a a a

data Deg
data Rad

newtype Angle :: Type -> Type
newtype Angle a = Angle Value

type role Angle nominal

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
