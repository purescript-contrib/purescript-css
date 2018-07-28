module CSS.Selector where

import Prelude
import CSS.Refinement (Refinement(..), byClass, byId)
import CSS.String (class IsString)
import Data.String (take, drop)

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
      _   -> Selector (Refinement []) (Elem s)

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
