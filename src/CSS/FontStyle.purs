module CSS.FontStyle where

import Prelude
import CSS.Common (class Inherit, class Initial, class Normal, class Unset)
import CSS.Property (class Val, Value, value)
import CSS.Size (Angle)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

data ObliqueValue
  = ObliqueDefault
  | ObliqueAngle Value

derive instance eqObliqueValue :: Eq ObliqueValue
derive instance ordObliqueValue :: Ord ObliqueValue

instance valObliqueValue :: Val ObliqueValue where
  value ObliqueDefault = fromString "oblique"
  value (ObliqueAngle v) = fromString "oblique " <> v

data FontStyle
  = Normal
  | Initial
  | Inherit
  | Unset
  | Italic
  | Oblique ObliqueValue

derive instance eqFontStyle :: Eq FontStyle
derive instance ordFontStyle :: Ord FontStyle

instance valFontStyle :: Val FontStyle where
  value Normal = fromString "normal"
  value Initial = fromString "initial"
  value Inherit = fromString "inherit"
  value Unset = fromString "unset"
  value Italic = fromString "italic"
  value (Oblique v) = value v

instance normalFontStyle :: Normal FontStyle where
  normal = Normal

instance initialFontStyle :: Initial FontStyle where
  initial = Initial

instance inheritFontStyle :: Inherit FontStyle where
  inherit = Inherit

instance unsetFontStyle :: Unset FontStyle where
  unset = Unset

italic :: FontStyle
italic = Italic

oblique :: FontStyle
oblique = Oblique ObliqueDefault

obliqueAngle :: forall a. Angle a -> FontStyle
obliqueAngle = Oblique <<< ObliqueAngle <<< value

fontStyle :: FontStyle -> CSS
fontStyle = key $ fromString "font-style"
