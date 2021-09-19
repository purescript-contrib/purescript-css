module CSS.ListStyle.Image where

import CSS.Common (class Inherit, class Initial, class None, class Unset, class URL)
import CSS.Property (class Val)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

data ListStyleImage
  = ListStyleImage String
  | Initial
  | Inherit
  | Unset
  | None

derive instance eqListStyleImage :: Eq ListStyleImage
derive instance ordListStyleImage :: Ord ListStyleImage

instance showListStyleImage :: Show ListStyleImage where
  show (ListStyleImage url) = "(ListStyleImage " <> show url <> ")"
  show Initial = "Initial"
  show Inherit = "Inherit"
  show Unset = "Unset"
  show None = "None"

instance valListStyleImage :: Val ListStyleImage where
  value (Initial) = fromString "initial"
  value (Inherit) = fromString "inherit"
  value (Unset) = fromString "unset"
  value (None) = fromString "none"
  value (ListStyleImage url) = fromString ("url('" <> url <> "')")

instance initialListStyleImage :: Initial ListStyleImage where
  initial = Initial

instance inheritListStyleImage :: Inherit ListStyleImage where
  inherit = Inherit

instance unsetListStyleImage :: Unset ListStyleImage where
  unset = Unset

instance noneListImageImage :: None ListStyleImage where
  none = None

instance urlListStyleImage :: URL ListStyleImage where
  url s = ListStyleImage s

listStyleImage :: ListStyleImage -> CSS
listStyleImage = key $ fromString "list-style-image"
