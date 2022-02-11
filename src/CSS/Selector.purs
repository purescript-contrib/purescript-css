module CSS.Selector where

import Prelude

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

newtype Refinement = Refinement (Array Predicate)

derive instance eqRefinement :: Eq Refinement
derive instance ordRefinement :: Ord Refinement

instance isStringRefinement :: IsString Refinement where
  fromString s =
    Refinement
      [ case take 1 s of
          "#" -> Id $ drop 1 s
          "." -> Class $ drop 1 s
          ":" -> Pseudo $ drop 1 s
          "@" -> Attr $ drop 1 s
          _ -> Attr s
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

data Selector = Selector Refinement (Path Selector)

derive instance eqSelector :: Eq Selector
derive instance ordSelector :: Ord Selector

instance isStringSelector :: IsString Selector where
  fromString s =
    case take 1 s of
      "#" -> Selector (byId $ drop 1 s) Star
      "." -> Selector (byClass $ drop 1 s) Star
      _ -> Selector (Refinement []) (Elem s)

-- | The star selector applies to all elements.
-- | Maps to `*` in CSS.
star :: Selector
star = Selector (Refinement []) Star

-- | Select elements by name.
element :: String -> Selector
element e = Selector (Refinement []) (Elem e)

-- | The deep selector composer.
-- | Maps to `sel1 sel2` in CSS.
deep :: Selector -> Selector -> Selector
deep a b = Selector (Refinement []) (Deep a b)

infix 6 deep as |*

-- | The child selector composer.
-- | Maps to `sel1 > sel2` in CSS.
child :: Selector -> Selector -> Selector
child a b = Selector (Refinement []) (PathChild a b)

infix 6 child as |>

-- | The adjacent selector composer.
-- | Maps to `sel1 + sel2` in CSS.
adjacent :: Selector -> Selector -> Selector
adjacent a b = Selector (Refinement []) (Adjacent a b)

infix 6 adjacent as |+

-- | The filter selector composer, adds a filter to a selector.
-- | Maps to something like `sel#filter`, `sel.filter` or `sel:filter` in CSS,
-- | depending on the filter.
with :: Selector -> Refinement -> Selector
with (Selector (Refinement fs) e) (Refinement ps) = Selector (Refinement (fs <> ps)) e

infix 6 with as &

-- | Filter elements by id.
byId :: String -> Refinement
byId = Refinement <<< pure <<< Id

-- | Filter elements by class.
byClass :: String -> Refinement
byClass = Refinement <<< pure <<< Class

-- | Filter elements by pseudo selector or pseudo class.
-- | The preferred syntax is to use `:pseudo-selector` or
-- | use one of the predefined ones from `CSS.Pseudo`.
pseudo :: String -> Refinement
pseudo = Refinement <<< pure <<< Pseudo

-- | Filter elements by pseudo selector functions.
-- | The preferred way is to use one of the predefined functions from `CSS.Pseudo`.
func :: String -> Array String -> Refinement
func f = Refinement <<< pure <<< PseudoFunc f

-- | Filter elements based on the presence of a certain attribute.
attr :: String -> Refinement
attr = Refinement <<< pure <<< Attr

-- | Filter elements based on the presence of a
-- | certain attribute with the specified value.
attrVal :: String -> String -> Refinement
attrVal a = Refinement <<< pure <<< AttrVal a

infix 6 attrVal as @=

-- | Filter elements based on the presence of a certain attribute that
-- | begins with the selected value.
attrBegins :: String -> String -> Refinement
attrBegins a = Refinement <<< pure <<< AttrBegins a

infix 6 attrBegins as ^=

-- | Filter elements based on the presence of a certain attribute that
-- | ends with the specified value.
attrEnds :: String -> String -> Refinement
attrEnds a = Refinement <<< pure <<< AttrEnds a

infix 6 attrEnds as $=

-- | Filter elements based on the presence of a certain attribute that contains
-- | the specified value as a substring.
attrContains :: String -> String -> Refinement
attrContains a = Refinement <<< pure <<< AttrContains a

infix 6 attrContains as *=

-- | Filter elements based on the presence of a certain attribute that
-- | have the specified value contained in a space separated list.
attrSpace :: String -> String -> Refinement
attrSpace a = Refinement <<< pure <<< AttrSpace a

infix 6 attrSpace as ~=

-- | Filter elements based on the presence of a certain attribute that
-- | have the specified value contained in a hyphen separated list.
attrHyph :: String -> String -> Refinement
attrHyph a = Refinement <<< pure <<< AttrHyph a

infix 6 attrHyph as |=
