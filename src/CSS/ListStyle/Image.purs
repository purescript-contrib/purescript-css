module CSS.ListStyle.Image where

import CSS.Common (class Inherit, class Initial, class None, class Unset, class URL)
import CSS.Property (class Val)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

data ListStyleImage
  = ListStyleImage String
  | Initial
  | Inherit
  | Unset
  | None

instance valListStyleImage :: Val ListStyleImage where
  value s = fromString (show s)

instance showListStyleImage :: Show ListStyleImage where
  show (Initial) = "initial"
  show (Inherit) = "inherit"
  show (Unset) = "unset"
  show (None) = "none"
  show (ListStyleImage url) = ("url('" <> url <> "')")

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
