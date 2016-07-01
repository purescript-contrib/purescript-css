module CSS.Size where

import Prelude

import Data.Generic (class Generic)

import CSS.Common (class Auto)
import CSS.Property (class Val, Value, value)
import CSS.String (class IsString, fromString)

newtype Size a = Size Value

derive instance eqSize :: Eq a => Eq (Size a)
derive instance ordSize :: Ord a => Ord (Size a)
derive instance genericSize :: Generic a => Generic (Size a)

instance isStringSize :: IsString (Size a) where
  fromString = Size <<< fromString

instance valSize :: Val (Size a) where
  value (Size v) = v

instance autoSize :: Auto (Size a) where
  auto = fromString "auto"

data Abs
data Rel

nil :: forall a. Size a
nil = Size $ fromString "0"

px :: Number -> Size Abs
px i = Size (value i <> fromString "px")

pt :: Number -> Size Abs
pt i = Size (value i <> fromString "pt")

em :: Number -> Size Abs
em i = Size (value i <> fromString "em")

ex :: Number -> Size Abs
ex i = Size (value i <> fromString "ex")

pct :: Number -> Size Rel
pct i = Size (value i <> fromString "%")

rem :: Number -> Size Rel
rem i = Size (value i <> fromString "rem")

vw :: Number -> Size Rel
vw i = Size (value i <> fromString "vw")

vh :: Number -> Size Rel
vh i = Size (value i <> fromString "vh")

vmin :: Number -> Size Rel
vmin i = Size (value i <> fromString "vmin")

vmax :: Number -> Size Rel
vmax i = Size (value i <> fromString "vmax")

sym :: forall a b. (a -> a -> a -> a -> b) -> a -> b
sym f a = f a a a a

data Deg
data Rad

newtype Angle a = Angle Value

derive instance eqAngle :: Eq a => Eq (Angle a)
derive instance ordAngle :: Ord a => Ord (Angle a)
derive instance genericAngle :: Generic a => Generic (Angle a)

instance valAngle :: Val (Angle a) where
  value (Angle v) = v

deg :: Number -> Angle Deg
deg i = Angle $ (value i <> fromString "deg")

rad :: Number -> Angle Rad
rad i = Angle $ (value i <> fromString "rad")
