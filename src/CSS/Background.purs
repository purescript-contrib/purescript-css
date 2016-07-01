module CSS.Background
  (
  -- * Generic background property.
    class Background
  , background

  -- * The background-color.
  , backgroundColor

  -- * The background-position.
  , BackgroundPosition
  , backgroundPosition
  , backgroundPositions
  , placed
  , positioned

  -- * The background-size.
  , BackgroundSize
  , backgroundSize
  , backgroundSizes
  , contain, cover
  , by

  -- * The background-repeat.
  , BackgroundRepeat
  , backgroundRepeat
  , backgroundRepeats
  , repeat, space, round, noRepeat
  , xyRepeat
  , repeatX, repeatY

  -- * The background-origin.
  , BackgroundOrigin
  , backgroundOrigin
  , backgroundOrigins
  , origin

  -- * The background-clip.
  , BackgroundClip
  , backgroundClip
  , backgroundClips
  , boxClip

  -- * The background-attachment.
  , BackgroundAttachment
  , backgroundAttachment
  , backgroundAttachments
  , attachFixed, attachScroll

  -- * The background-image.
  , BackgroundImage
  , backgroundImage
  , backgroundImages
  , url

  -- * Specifying sides.
  , Side
  , sideTop
  , sideLeft
  , sideRight
  , sideBottom
  , sideCenter
  , sideMiddle

  -- * Specifying directions and location.
  , Direction
  , straight
  , angular

  , Location
  , class Loc
  , location
  )
where

import Prelude

import Data.Generic (class Generic)
import Data.Tuple (Tuple(..))

import CSS.Box (BoxType)
import CSS.Color (Color)
import CSS.Common (class Other, class Inherit, class None, class Auto, auto)
import CSS.Property (class Val, Value, value)
import CSS.Size (Size, Angle)
import CSS.String (class IsString, fromString)
import CSS.Stylesheet (CSS, key)

-- | We implement the generic background property as a type class that accepts
-- multiple value types. This allows us to combine different background aspects
-- into a shorthand syntax.

class Val a <= Background a where
  background :: a -> CSS

instance backgroundArray :: (Background a) => Background (Array a) where
  background = key $ fromString "background"

instance backgroundTuple
  :: (Background a, Background b) => Background (Tuple a b) where
  background = key $ fromString "background"

instance backgroundColor' :: Background Color where
  background = key $ fromString "background"

instance backgroundPosition' :: Background BackgroundPosition where
  background = key $ fromString "background"

instance backgroundSize' :: Background BackgroundSize where
  background = key $ fromString "background"

instance backgroundBackgroundRepeat :: Background BackgroundRepeat where
  background = key $ fromString "background"

instance backgroundBackgroundOrigin :: Background BackgroundOrigin where
  background = key $ fromString "background"

instance backgroundBackgroundClip :: Background BackgroundClip where
  background = key $ fromString "background"

instance backgroundBackgroundAttachment :: Background BackgroundAttachment
  where background = key $ fromString "background"

instance backgroundBackgroundImage :: Background BackgroundImage where
  background = key $ fromString "background"

-------------------------------------------------------------------------------

backgroundColor :: Color -> CSS
backgroundColor = key $ fromString "background-color"

-------------------------------------------------------------------------------

newtype BackgroundPosition = BackgroundPosition Value

derive instance eqBackgroundPosition :: Eq BackgroundPosition
derive instance ordBackgroundPosition :: Ord BackgroundPosition
derive instance genericBackgroundPosition :: Generic BackgroundPosition

instance isStringBackgroundPosition :: IsString BackgroundPosition where
  fromString = BackgroundPosition <<< fromString

instance valBackgroundPosition :: Val BackgroundPosition where
  value (BackgroundPosition v) = v

instance otherBackgroundPosition :: Other BackgroundPosition where
  other = BackgroundPosition

instance inheritBackgroundPosition :: Inherit BackgroundPosition where
  inherit = fromString "inherit"

placed :: Side -> Side -> BackgroundPosition
placed a b = BackgroundPosition (value (Tuple a b))

positioned :: forall a. Size a -> Size a -> BackgroundPosition
positioned a b = BackgroundPosition (value (Tuple a b))

backgroundPosition :: BackgroundPosition -> CSS
backgroundPosition = key $ fromString "background-position"

backgroundPositions :: Array BackgroundPosition -> CSS
backgroundPositions = key $ fromString "background-position"

-------------------------------------------------------------------------------

newtype BackgroundSize = BackgroundSize Value

derive instance eqBackgroundSize :: Eq BackgroundSize
derive instance ordBackgroundSize :: Ord BackgroundSize
derive instance genericBackgroundSize :: Generic BackgroundSize

instance isStringBackgroundSize :: IsString BackgroundSize where
  fromString = BackgroundSize <<< fromString

instance valBackgroundSize :: Val BackgroundSize where
  value (BackgroundSize v) = v

instance otherBackgroundSize :: Other BackgroundSize where
  other = BackgroundSize

instance inheritBackgroundSize :: Inherit BackgroundSize where
  inherit = fromString "inherit"

instance autoBackgroundSize :: Auto BackgroundSize where
  auto = auto `by` auto

contain :: BackgroundSize
contain = BackgroundSize $ fromString "contain"

cover :: BackgroundSize
cover = BackgroundSize $ fromString "cover"

by :: forall a b. Size a -> Size b -> BackgroundSize
by a b = BackgroundSize (value (Tuple a b))

backgroundSize :: BackgroundSize -> CSS
backgroundSize = key $ fromString "background-size"

backgroundSizes :: Array BackgroundSize -> CSS
backgroundSizes = key $ fromString "background-size"

-------------------------------------------------------------------------------

newtype BackgroundRepeat = BackgroundRepeat Value

derive instance eqBackgroundRepeat :: Eq BackgroundRepeat
derive instance ordBackgroundRepeat :: Ord BackgroundRepeat
derive instance genericBackgroundRepeat :: Generic BackgroundRepeat

instance isStringBackgroundRepeat :: IsString BackgroundRepeat where
  fromString = BackgroundRepeat <<< fromString

instance valBackgroundRepeat :: Val BackgroundRepeat where
  value (BackgroundRepeat v) = v

instance otherBackgroundRepeat :: Other BackgroundRepeat where
  other = BackgroundRepeat

instance inheritBackgroundRepeat :: Inherit BackgroundRepeat where
  inherit = fromString "inherit"

instance noneBackgroundRepeat :: None BackgroundRepeat where
  none = fromString "none"

repeat :: BackgroundRepeat
repeat = BackgroundRepeat $ fromString "repeat"

space :: BackgroundRepeat
space = BackgroundRepeat $ fromString "space"

round :: BackgroundRepeat
round = BackgroundRepeat $ fromString "round"

noRepeat :: BackgroundRepeat
noRepeat = BackgroundRepeat $ fromString "no-repeat"

xyRepeat :: BackgroundRepeat -> BackgroundRepeat -> BackgroundRepeat
xyRepeat a b = BackgroundRepeat (value (Tuple a b))

repeatX :: BackgroundRepeat
repeatX = xyRepeat repeat noRepeat

repeatY :: BackgroundRepeat
repeatY = xyRepeat noRepeat repeat

backgroundRepeat :: BackgroundRepeat -> CSS
backgroundRepeat = key $ fromString "background-repeat"

backgroundRepeats :: Array BackgroundRepeat -> CSS
backgroundRepeats = key $ fromString "background-repeat"

-------------------------------------------------------------------------------

newtype BackgroundImage = BackgroundImage Value

derive instance eqBackgroundImage :: Eq BackgroundImage
derive instance ordBackgroundImage :: Ord BackgroundImage
derive instance genericBackgroundImage :: Generic BackgroundImage

instance isStringBackgroundImage :: IsString BackgroundImage where
  fromString = BackgroundImage <<< fromString

instance valBackgroundImage :: Val BackgroundImage where
  value (BackgroundImage v) = v

instance otherBackgroundImage :: Other BackgroundImage where
  other = BackgroundImage

instance inheritBackgroundImage :: Inherit BackgroundImage where
  inherit = fromString "inherit"

instance noneBackgroundImage :: None BackgroundImage where
  none = fromString "none"

url :: String -> BackgroundImage
url u = BackgroundImage (value ("url(\"" <> u <> "\")"))

backgroundImage :: BackgroundImage -> CSS
backgroundImage = key $ fromString "background-image"

backgroundImages :: Array BackgroundImage -> CSS
backgroundImages = key $ fromString "background-image"

-------------------------------------------------------------------------------

newtype BackgroundOrigin = BackgroundOrigin Value

derive instance eqBackgroundOrigin :: Eq BackgroundOrigin
derive instance ordBackgroundOrigin :: Ord BackgroundOrigin
derive instance genericBackgroundOrigin :: Generic BackgroundOrigin

instance isStringBackgroundOrigin :: IsString BackgroundOrigin where
  fromString = BackgroundOrigin <<< fromString

instance valBackgroundOrigin :: Val BackgroundOrigin where
  value (BackgroundOrigin v) = v

instance otherBackgroundOrigin :: Other BackgroundOrigin where
  other = BackgroundOrigin

instance inheritBackgroundOrigin :: Inherit BackgroundOrigin where
  inherit = fromString "inherit"

origin :: BoxType -> BackgroundOrigin
origin b = BackgroundOrigin (value b)

backgroundOrigin :: BackgroundOrigin -> CSS
backgroundOrigin = key $ fromString "background-origin"

backgroundOrigins :: Array BackgroundOrigin -> CSS
backgroundOrigins = key $ fromString "background-origin"

-------------------------------------------------------------------------------

newtype BackgroundClip = BackgroundClip Value

derive instance eqBackgroundClip :: Eq BackgroundClip
derive instance ordBackgroundClip :: Ord BackgroundClip
derive instance genericBackgroundClip :: Generic BackgroundClip

instance isStringBackgroundClip :: IsString BackgroundClip where
  fromString = BackgroundClip <<< fromString

instance valBackgroundClip :: Val BackgroundClip where
  value (BackgroundClip v) = v

instance otherBackgroundClip :: Other BackgroundClip where
  other = BackgroundClip

instance inheritBackgroundClip :: Inherit BackgroundClip where
  inherit = fromString "inherit"

boxClip :: BoxType -> BackgroundClip
boxClip b = BackgroundClip (value b)

backgroundClip :: BackgroundClip -> CSS
backgroundClip = key $ fromString "background-clip"

backgroundClips :: Array BackgroundClip -> CSS
backgroundClips = key $ fromString "background-clip"

-------------------------------------------------------------------------------

newtype BackgroundAttachment = BackgroundAttachment Value

derive instance eqBackgroundAttachment :: Eq BackgroundAttachment
derive instance ordBackgroundAttachment :: Ord BackgroundAttachment
derive instance genericBackgroundAttachment :: Generic BackgroundAttachment

instance isStringBackgroundAttachment :: IsString BackgroundAttachment where
  fromString = BackgroundAttachment <<< fromString

instance valBackgroundAttachment :: Val BackgroundAttachment where
  value (BackgroundAttachment v) = v

instance otherBackgroundAttachment :: Other BackgroundAttachment where
  other = BackgroundAttachment

instance inheritBackgroundAttachment :: Inherit BackgroundAttachment where
  inherit = fromString "inherit"

attachFixed :: BackgroundAttachment
attachFixed = BackgroundAttachment $ fromString "fixed"

attachScroll :: BackgroundAttachment
attachScroll = BackgroundAttachment $ fromString "scroll"

backgroundAttachment :: BackgroundAttachment -> CSS
backgroundAttachment = key $ fromString "background-attachment"

backgroundAttachments :: Array BackgroundAttachment -> CSS
backgroundAttachments = key $ fromString "background-attachment"

-------------------------------------------------------------------------------

newtype Side = Side Value

derive instance eqSide :: Eq Side
derive instance ordSide :: Ord Side
derive instance genericSide :: Generic Side

instance isStringSide :: IsString Side where
  fromString = Side <<< fromString

instance valSide :: Val Side where
  value (Side v) = v

instance otherSide :: Other Side where
  other = Side

instance inheritSide :: Inherit Side where
  inherit = fromString "inherit"

-- | We have to prefix these values to avoid conflict with existing property
-- names.

sideTop :: Side
sideTop = Side $ fromString "top"

sideLeft :: Side
sideLeft = Side $ fromString "left"

sideRight :: Side
sideRight = Side $ fromString "right"

sideBottom :: Side
sideBottom = Side $ fromString "bottom"

sideCenter :: Side
sideCenter = Side $ fromString "center"

sideMiddle :: Side
sideMiddle = Side $ fromString "middle"

-------------------------------------------------------------------------------

newtype Direction = Direction Value

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction
derive instance genericDirection :: Generic Direction

instance valDirection :: Val Direction where
  value (Direction v) = v

instance otherDirection :: Other Direction where
  other = Direction

straight :: Side -> Direction
straight a = Direction (value a)

angular :: forall a. Angle a -> Direction
angular a = Direction (value a)

newtype Location = Location Value

derive instance eqLocation :: Eq Location
derive instance ordLocation :: Ord Location
derive instance genericLocation :: Generic Location

instance valLocation :: Val Location where
  value (Location v) = v

instance otherLocation :: Other Location where
  other = Location

class Val a <= Loc a where
  location :: a -> Location

instance locSide :: Loc Side where
  location = Location <<< value

instance locSize :: Loc (Size a) where
  location = Location <<< value

instance locTuple :: (Loc a, Loc b) => Loc (Tuple a b) where
  location = Location <<< value
