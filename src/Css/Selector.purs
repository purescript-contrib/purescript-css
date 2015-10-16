module Css.Selector where

import Prelude

import Data.Generic
import Data.String (take, drop)

import Css.String

data Predicate = Id String
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

derive instance genericPredicate :: Generic Predicate

instance eqPredicate :: Eq Predicate where
  eq = gEq

instance ordPredicate :: Ord Predicate where
  compare = gCompare

newtype Refinement = Refinement (Array Predicate)

instance isStringRefinement :: IsString Refinement where
  fromString s = Refinement [ case take 1 s of
                                 "#" -> Id $ drop 1 s
                                 "." -> Class $ drop 1 s
                                 ":" -> Pseudo $ drop 1 s
                                 "@" -> Attr $ drop 1 s
                                 _   -> Attr s
                            ]

data Path f = Star
            | Elem String
            | PathChild f f
            | Deep      f f
            | Adjacent  f f
            | Combined  f f

data Selector = Selector Refinement (Path Selector)

instance isStringSelector :: IsString Selector where
  fromString s = case take 1 s of
                   "#" -> Selector (Refinement [Id $ drop 1 s]) Star
                   "." -> Selector (Refinement [Class $ drop 1 s]) Star
                   _   -> Selector (Refinement []) (Elem s)

star :: Selector
star = Selector (Refinement []) Star

element :: String -> Selector
element e = Selector (Refinement []) (Elem e)

deep :: Selector -> Selector -> Selector
deep a b = Selector (Refinement []) (Deep a b)

(**) :: Selector -> Selector -> Selector
(**) = deep

child :: Selector -> Selector -> Selector
child a b = Selector (Refinement []) (PathChild a b)

(|>) :: Selector -> Selector -> Selector
(|>) = child

with :: Selector -> Refinement -> Selector
with (Selector (Refinement fs) e) (Refinement ps) = Selector (Refinement (fs ++ ps)) e

(##) :: Selector -> Refinement -> Selector
(##) = with
