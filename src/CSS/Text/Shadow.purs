module CSS.Text.Shadow where

import CSS.Color (Color)
import CSS.Common (class Inherit, class Initial, class None)
import CSS.Property (class Val, value)
import CSS.Size (Size)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Tuple.Nested (tuple4)

data TextShadow a
  = TextShadow (Size a) (Size a) (Size a) (Color)
  | None
  | Initial
  | Inherit

instance valTextShadow :: Val (TextShadow a) where
  value (TextShadow h v b c) = value (tuple4 h v b c)
  value (None) = fromString "none"
  value (Initial) = fromString "initial"
  value (Inherit) = fromString "inherit"

instance noneTextShadow :: None (TextShadow a) where
  none = None

instance initialTextShadow :: Initial (TextShadow a) where
  initial = Initial

instance inheritTextShadow :: Inherit (TextShadow a) where
  inherit = Inherit

textShadow :: forall a. Size a -> Size a -> Size a -> Color -> CSS
textShadow h v b c = key (fromString "text-shadow") (TextShadow h v b c)
