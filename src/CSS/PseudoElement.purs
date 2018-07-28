module CSS.PseudoElement where

import CSS.String (class IsString)
import Data.Eq (class Eq)
import Data.Ord (class Ord)

data PseudoElement
  = After
  | Before
  | FirstLetter
  | LastLetter
  | FirstLine
  | Selection
  | Backdrop
  | Raw String

derive instance eqPseudoElement :: Eq PseudoElement
derive instance ordPseudoElement :: Ord PseudoElement

pseudoElementName :: PseudoElement -> String
pseudoElementName = case _ of
  After -> "after"
  Before -> "before"
  FirstLetter -> "first-letter"
  LastLetter -> "last-letter"
  FirstLine -> "first-line"
  Selection -> "selection"
  Backdrop -> "backdrop"
  Raw s -> s

instance isStringPseudoElement :: IsString PseudoElement where
  fromString = case _ of
    "after" -> After
    "before" -> Before
    "first-letter" -> FirstLetter
    "last-letter" -> LastLetter
    "first-line" -> FirstLine
    "selection" -> Selection
    "backdrop" -> Backdrop
    s -> Raw s
