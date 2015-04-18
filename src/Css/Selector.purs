module Css.Selector where

import Css.String
import Data.Monoid () -- Gross, forall a. [a] has an orphan I guess.
import qualified Data.String as S

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
               | PseudoFunc String [String]

instance eqPredicate :: Eq Predicate where
  (==) (Id a) (Id b) = a == b
  (/=) a b = not (a == b)

instance ordPredicate :: Ord Predicate where
  compare (Id a) (Id b) = compare a b

newtype Refinement = Refinement [Predicate]

instance isStringRefinement :: IsString Refinement where
  fromString s = Refinement [ case S.take 1 s of
                                 "#" -> Id $ S.drop 1 s
                                 "." -> Class $ S.drop 1 s
                                 ":" -> Pseudo $ S.drop 1 s
                                 "@" -> Attr $ S.drop 1 s
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
  fromString s = case S.take 1 s of
                   "#" -> Selector (Refinement [Id $ S.drop 1 s]) Star
                   "." -> Selector (Refinement [Class $ S.drop 1 s]) Star
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
