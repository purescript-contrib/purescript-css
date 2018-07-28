module CSS.Predicate where

import CSS.PseudoClass (PseudoClass)
import CSS.PseudoElement (PseudoElement)
import Data.Eq (class Eq)
import Data.Ord (class Ord)

data Predicate
  = Id String
  | Class String
  | Attr String
  | AttrVal String String
  | AttrBegins String String
  | AttrEnds String String
  | AttrContains String String
  | AttrSpace String String
  | AttrHyph String String
  | PseudoFunc String (Array String)
  | PseudoClass PseudoClass
  | PseudoElement PseudoElement

derive instance eqPredicate :: Eq Predicate
derive instance ordPredicate :: Ord Predicate
