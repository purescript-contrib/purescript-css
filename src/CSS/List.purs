module CSS.List where

import Prelude
import CSS.Common (class Inherit, inherit, class Initial, class None, initial, none)
import CSS.Property (class Val, Value, value)
import CSS.String (fromString)
import CSS.Stylesheet (key, CSS)
import Data.Generic (class Generic)

newtype ListStyleType = ListStyleType Value

derive instance eqPosition :: Eq ListStyleType
derive instance ordPosition :: Ord ListStyleType
derive instance genericPosition :: Generic ListStyleType

instance valListStyleType :: Val ListStyleType where
  value (ListStyleType v) = v

instance initialListStyleType :: Initial ListStyleType where
  initial = ListStyleType initial

instance inheritListStyleType :: Inherit ListStyleType where
  inherit = ListStyleType inherit

instance noneListStyleType :: None ListStyleType where
  none = ListStyleType none

listStyleType :: ListStyleType -> CSS
listStyleType = key $ fromString "list-style-type"

disc :: ListStyleType
disc = ListStyleType $ fromString "disc"
armenian :: ListStyleType
armenian = ListStyleType $ fromString "armenian"
circleListStyle :: ListStyleType
circleListStyle = ListStyleType $ fromString "circle"
cjkIdeographic :: ListStyleType
cjkIdeographic = ListStyleType $ fromString "cjk-ideographic"
decimal :: ListStyleType
decimal = ListStyleType $ fromString "decimal"
decimalLeadingZero :: ListStyleType
decimalLeadingZero = ListStyleType $ fromString "decimal-leading-zero"
georgian :: ListStyleType
georgian = ListStyleType $ fromString "georgian"
hebrew :: ListStyleType
hebrew = ListStyleType $ fromString "hebrew"
hiragana :: ListStyleType
hiragana = ListStyleType $ fromString "hiragana"
hiraganaIroha :: ListStyleType
hiraganaIroha = ListStyleType $ fromString "hiragana-iroha"
katakana :: ListStyleType
katakana = ListStyleType $ fromString "katakana"
katakanaIroha :: ListStyleType
katakanaIroha = ListStyleType $ fromString "katakana-iroha"
lowerAlpha :: ListStyleType
lowerAlpha = ListStyleType $ fromString "lower-alpha"
lowerGreek :: ListStyleType
lowerGreek = ListStyleType $ fromString "lower-greek"
lowerLatin :: ListStyleType
lowerLatin = ListStyleType $ fromString "lower-latin"
lowerRoman :: ListStyleType
lowerRoman = ListStyleType $ fromString "lower-roman"
square :: ListStyleType
square = ListStyleType $ fromString "square"
upperAlpha :: ListStyleType
upperAlpha = ListStyleType $ fromString "upper-alpha"
upperLatin :: ListStyleType
upperLatin = ListStyleType $ fromString "upper-latin"
upperRoman :: ListStyleType
upperRoman = ListStyleType $ fromString "upper-roman"

newtype ListStylePosition = ListStylePosition Value

derive instance eqListStylePosition :: Eq ListStylePosition
derive instance ordListStylePosition :: Ord ListStylePosition
derive instance genericListStylePosition :: Generic ListStylePosition

instance valListStylePosition :: Val ListStylePosition where
  value (ListStylePosition v) = v

instance initialListStylePosition :: Initial ListStylePosition where
  initial = ListStylePosition initial

instance inheritListStylePosition :: Inherit ListStylePosition where
  inherit = ListStylePosition inherit

listStylePosition :: ListStylePosition -> CSS
listStylePosition = key $ fromString "list-style-position"

inside :: ListStylePosition
inside  = ListStylePosition (value "inside")
outside :: ListStylePosition
outside = ListStylePosition (value "outside")

newtype ListStyleImage = ListStyleImage Value

derive instance eqListStyleImage :: Eq ListStyleImage
derive instance ordListStyleImage :: Ord ListStyleImage
derive instance genericListStyleImage :: Generic ListStyleImage

instance valListStyleImage :: Val ListStyleImage where
  value (ListStyleImage v) = v

instance initialListStyleImage :: Initial ListStyleImage where
  initial = ListStyleImage initial

instance inheritListStyleImage :: Inherit ListStyleImage where
  inherit = ListStyleImage inherit

instance noneListStyleImage :: None ListStyleImage where
  none = ListStyleImage none

listStyleImage :: ListStyleImage -> CSS
listStyleImage = key $ fromString "list-style-image"

imageUrl :: String -> ListStyleImage
imageUrl u = ListStyleImage $ fromString ("url(" <> u <> ")")
