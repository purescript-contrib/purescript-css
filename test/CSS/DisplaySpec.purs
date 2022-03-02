module CSS.DisplaySpec where

import Prelude

import CSS.Color (green)
import CSS.Color as Color
import CSS.Common (hidden, inherit, initial, unset, visible)
import CSS.Display (collapse, visibility)
import CSS.Size (em, px)
import Common (shouldRenderFrom)
import Data.Maybe (fromJust)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = do

  describe "visibility (Mozilla examples)" do
    let testVisibility (s /\ v) = ("visibility: " <> s) `shouldRenderFrom` visibility v
    describe "Keyword values" $
      traverse_
        testVisibility
        [ "visible" /\ visible
        , "hidden" /\ hidden
        , "collapse" /\ collapse
        ]
    describe "Global values" $
      traverse_
        testVisibility
        [ "inherit" /\ inherit
        , "initial" /\ initial
        , "unset" /\ unset
        ]
