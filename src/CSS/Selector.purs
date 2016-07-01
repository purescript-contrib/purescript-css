module CSS.Selector where

import Prelude

import Data.Generic (class Generic)
import Data.String (take, drop)

import CSS.String (class IsString)

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
  | Pseudo String
  | PseudoFunc String (Array String)

derive instance eqPredicate :: Eq Predicate
derive instance ordPredicate :: Ord Predicate
derive instance genericPredicate :: Generic Predicate

newtype Refinement = Refinement (Array Predicate)

derive instance eqRefinement :: Eq Refinement
derive instance ordRefinement :: Ord Refinement
derive instance genericRefinement :: Generic Refinement

instance isStringRefinement :: IsString Refinement where
  fromString s =
    Refinement
      [ case take 1 s of
          "#" -> Id $ drop 1 s
          "." -> Class $ drop 1 s
          ":" -> Pseudo $ drop 1 s
          "@" -> Attr $ drop 1 s
          _   -> Attr s
      ]

data Path f
  = Star
  | Elem String
  | PathChild f f
  | Deep f f
  | Adjacent f f
  | Combined f f

derive instance eqPath :: (Eq f) => Eq (Path f)
derive instance ordPath :: (Ord f) => Ord (Path f)
derive instance genericPath :: (Generic f) => Generic (Path f)

data Selector = Selector Refinement (Path Selector)

derive instance eqSelector :: Eq Selector
derive instance ordSelector :: Ord Selector
derive instance genericSelector :: Generic Selector

instance isStringSelector :: IsString Selector where
  fromString s =
    case take 1 s of
      "#" -> Selector (Refinement [Id $ drop 1 s]) Star
      "." -> Selector (Refinement [Class $ drop 1 s]) Star
      _   -> Selector (Refinement []) (Elem s)

star :: Selector
star = Selector (Refinement []) Star

element :: String -> Selector
element e = Selector (Refinement []) (Elem e)

deep :: Selector -> Selector -> Selector
deep a b = Selector (Refinement []) (Deep a b)
infix 0 deep as **

child :: Selector -> Selector -> Selector
child a b = Selector (Refinement []) (PathChild a b)
infix 0 child as |>

with :: Selector -> Refinement -> Selector
with (Selector (Refinement fs) e) (Refinement ps) = Selector (Refinement (fs <> ps)) e
infix 0 child as ##
