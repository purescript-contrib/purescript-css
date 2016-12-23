module CSS.Text.Transform
  ( TextTransform
  , textTransform
  , lowercase
  , uppercase
  , capitalize
  ) where

import CSS.Common (class Inherit, class Initial, class None)
import CSS.Property (class Val, value)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic (class Generic)
import Data.Ord (class Ord)
import Data.Show (class Show, show)

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
  value t = value (show t)

instance showTextTransform :: Show TextTransform where
  show (Uppercase)  = "uppercase"
  show (Lowercase)  = "lowercase"
  show (Capitalize) = "capitalize"
  show (None)       = "none"
  show (Initial)    = "initial"
  show (Inherit)    = "inherit"

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
