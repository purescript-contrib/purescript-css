module CSS.ListStyle.Position where

import CSS.Common (class Inherit, class Initial, class Unset)
import CSS.Property (class Val)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Function (($))
import Data.Show (class Show, show)

data ListStylePosition
  = Inside
  | Outside
  | Inherit
  | Initial
  | Unset

instance valListStylePosition :: Val ListStylePosition where
  value s = fromString (show s)

instance showListStylePosition :: Show ListStylePosition where
  show (Inside) = "inside"
  show (Outside) = "outside"
  show (Inherit) = "inherit"
  show (Initial) = "initial"
  show (Unset) = "unset"

instance initialListStylePosition :: Initial ListStylePosition where
  initial = Initial

instance inheritListStylePosition :: Inherit ListStylePosition where
  inherit = Inherit

instance unsetListStylePosition :: Unset ListStylePosition where
  unset = Unset

inside :: ListStylePosition
inside = Inside

outside :: ListStylePosition
outside = Outside

listStylePosition :: ListStylePosition -> CSS
listStylePosition = key $ fromString "list-style-position"
