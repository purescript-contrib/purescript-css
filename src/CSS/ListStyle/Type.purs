module CSS.ListStyle.Type where

import CSS.Common (class Inherit, class Initial, class None, class Unset)
import CSS.Property (class Val)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

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

instance valListStyleType :: Val ListStyleType where
  value s = fromString (show s)

instance showListStyleType :: Show ListStyleType where
  show (Disc) = "disc"
  show (Circle) = "circle"
  show (Square) = "square"
  show (Decimal) = "decimal"
  show (Georgian) = "georgian"
  show (CJKIdeographic) = "cjk-ideographic"
  show (Kannada) = "kannada"
  show (None) = "none"
  show (Initial) = "initial"
  show (Inherit) = "inherit"
  show (Unset) = "unset"
  show (CustomStyleType s) = ("custom-" <> s)
  show (StringStyleType s) = s

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
