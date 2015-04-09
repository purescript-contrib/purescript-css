module Css.Selector where

-- Gross, forall a. [a] has an orphan I guess.
import Data.Monoid ()

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

newtype Refinement = Refinement [Predicate]

data Path f = Star
            | Elem String
            | PathChild f f
            | Deep      f f
            | Adjacent  f f
            | Combined  f f

data Selector = Selector Refinement (Path Selector)

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
