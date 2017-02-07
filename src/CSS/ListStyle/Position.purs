module CSS.ListStyle.Position where

import CSS.Common (class Inherit, class Initial, class Unset)
import CSS.Property (class Val)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic (class Generic, gShow)
import Data.Ord (class Ord)
import Data.Show (class Show)

data ListStylePosition
  = Inside
  | Outside
  | Inherit
  | Initial
  | Unset

derive instance eqListStylePosition :: Eq ListStylePosition
derive instance ordListStylePosition :: Ord ListStylePosition
derive instance genericListStylePosition :: Generic ListStylePosition

instance showListStylePosition :: Show ListStylePosition where
  show = gShow

instance valListStylePosition :: Val ListStylePosition where
  value (Inside) = fromString "inside"
  value (Outside) = fromString "outside"
  value (Inherit) = fromString "inherit"
  value (Initial) = fromString "initial"
  value (Unset) = fromString "unset"

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
