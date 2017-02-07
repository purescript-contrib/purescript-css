module CSS.ListStyle.Type where

import CSS.Common (class Inherit, class Initial, class None, class Unset)
import CSS.Property (class Val)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic (class Generic, gShow)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (class Show)

data ListStyleType
  = Disc
  | Circle
  | Square
  | Decimal
  | Georgian
  | CJKIdeographic
  | Kannada
  | None
  | Inherit
  | Initial
  | Unset
  | CustomStyleType String
  | StringStyleType String

derive instance eqListStyleType :: Eq ListStyleType
derive instance ordListStyleType :: Ord ListStyleType
derive instance genericListStyleType :: Generic ListStyleType

instance showListStyleType :: Show ListStyleType where
  show = gShow

instance valListStyleType :: Val ListStyleType where
  value (Disc) = fromString "disc"
  value (Circle) = fromString "circle"
  value (Square) = fromString "square"
  value (Decimal) = fromString "decimal"
  value (Georgian) = fromString "georgian"
  value (CJKIdeographic) = fromString "cjk-ideographic"
  value (Kannada) = fromString "kannada"
  value (None) = fromString "none"
  value (Initial) = fromString "initial"
  value (Inherit) = fromString "inherit"
  value (Unset) = fromString "unset"
  value (CustomStyleType s) = fromString ("custom-" <> s)
  value (StringStyleType s) = fromString s

instance initialListStyleType :: Initial ListStyleType where
  initial = Initial

instance inheritListStyleType :: Inherit ListStyleType where
  inherit = Inherit

instance unsetListStyleType :: Unset ListStyleType where
  unset = Unset

instance noneListTypeType :: None ListStyleType where
  none = None

disc :: ListStyleType
disc = Disc

circle :: ListStyleType
circle = Circle

square :: ListStyleType
square = Square

decimal :: ListStyleType
decimal = Decimal

georgian :: ListStyleType
georgian = Georgian

cjkIdeographic :: ListStyleType
cjkIdeographic = CJKIdeographic

kannada :: ListStyleType
kannada = Kannada

customListStyleType :: String -> ListStyleType
customListStyleType s = CustomStyleType s

stringListStyleType :: String -> ListStyleType
stringListStyleType s = StringStyleType s

listStyleType :: ListStyleType -> CSS
listStyleType = key $ fromString "list-style-type"
