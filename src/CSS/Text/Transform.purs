module CSS.Text.Transform
  ( TextTransform
  , textTransform
  , lowercase
  , uppercase
  , capitalize
  ) where

import CSS.Common (class Inherit, class Initial, class None)
import CSS.Property (class Val)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic (class Generic, gShow)
import Data.Ord (class Ord)
import Data.Show (class Show)

data TextTransform
  = Uppercase
  | Lowercase
  | Capitalize
  | None
  | Initial
  | Inherit

derive instance eqTextTransform :: Eq TextTransform
derive instance ordTextTransform :: Ord TextTransform
derive instance genericTextTransform :: Generic TextTransform

instance valTextTransform :: Val TextTransform where
  value (Uppercase)  = fromString "uppercase"
  value (Lowercase)  = fromString "lowercase"
  value (Capitalize) = fromString "capitalize"
  value (None)       = fromString "none"
  value (Initial)    = fromString "initial"
  value (Inherit)    = fromString "inherit"

instance showTextTransform :: Show (TextTransform) where
  show = gShow

instance noneTextTransform :: None TextTransform where
  none = None

instance initialTextTransform :: Initial TextTransform where
  initial = Initial

instance inheritTextTransform :: Inherit TextTransform where
  inherit = Inherit

textTransform :: TextTransform -> CSS
textTransform = key $ fromString "text-transform"

uppercase :: TextTransform
uppercase = Uppercase

lowercase :: TextTransform
lowercase = Lowercase

capitalize :: TextTransform
capitalize = Capitalize
