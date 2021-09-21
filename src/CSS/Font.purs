module CSS.Font where

import Prelude
import CSS.Color (Color)
import CSS.Common (class Inherit, class Initial, class Normal, class Unset)
import CSS.Property (class Val, Value, value, quote)
import CSS.Size (Size)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.NonEmpty (NonEmpty, oneOf)

color :: Color -> CSS
color = key $ fromString "color"

newtype GenericFontFamily = GenericFontFamily Value

derive instance eqGenericFontFamily :: Eq GenericFontFamily
derive instance ordGenericFontFamily :: Ord GenericFontFamily

instance valGenericFontFamily :: Val GenericFontFamily where
  value (GenericFontFamily v) = v

serif :: GenericFontFamily
serif = GenericFontFamily $ fromString "serif"

sansSerif :: GenericFontFamily
sansSerif = GenericFontFamily $ fromString "sans-serif"

cursive :: GenericFontFamily
cursive = GenericFontFamily $ fromString "cursive"

monospace :: GenericFontFamily
monospace = GenericFontFamily $ fromString "monospace"

fantasy :: GenericFontFamily
fantasy = GenericFontFamily $ fromString "fantasy"

systemUi :: GenericFontFamily
systemUi = GenericFontFamily $ fromString "system-ui"

uiSerif :: GenericFontFamily
uiSerif = GenericFontFamily $ fromString "ui-serif"

uiSansSerif :: GenericFontFamily
uiSansSerif = GenericFontFamily $ fromString "ui-sans-serif"

uiMonospace :: GenericFontFamily
uiMonospace = GenericFontFamily $ fromString "ui-monospace"

uiRounded :: GenericFontFamily
uiRounded = GenericFontFamily $ fromString "ui-rounded"

emoji :: GenericFontFamily
emoji = GenericFontFamily $ fromString "emoji"

math :: GenericFontFamily
math = GenericFontFamily $ fromString "math"

fangsong :: GenericFontFamily
fangsong = GenericFontFamily $ fromString "fangsong"

fontFamily :: Array String -> NonEmpty Array GenericFontFamily -> CSS
fontFamily a b = key (fromString "font-family") <<< value $ (value <<< quote <$> a) <> oneOf (value <$> b)

fontSize :: forall a. Size a -> CSS
fontSize = key $ fromString "font-size"

newtype FontWeight = FontWeight Value

derive instance eqFontWeight :: Eq FontWeight
derive instance ordFontWeight :: Ord FontWeight

instance valFontWeight :: Val FontWeight where
  value (FontWeight v) = v

instance normalFontWeight :: Normal FontWeight where
  normal = FontWeight (fromString "normal")

instance initialFontWeight :: Initial FontWeight where
  initial = FontWeight (fromString "initial")

instance inheritFontWeight :: Inherit FontWeight where
  inherit = FontWeight (fromString "inherit")

instance unsetFontWeight :: Unset FontWeight where
  unset = FontWeight (fromString "unset")

bold :: FontWeight
bold = FontWeight $ fromString "bold"

bolder :: FontWeight
bolder = FontWeight $ fromString "bolder"

lighter :: FontWeight
lighter = FontWeight $ fromString "lighter"

weight :: Number -> FontWeight
weight i = FontWeight $ value i

fontWeight :: FontWeight -> CSS
fontWeight = key $ fromString "font-weight"
