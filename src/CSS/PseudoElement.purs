module CSS.PseudoElement where

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
