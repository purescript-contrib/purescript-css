module CSS.DisplaySpec where

import Prelude

import CSS.Common (hidden, inherit, initial, unset, visible)
import CSS.Display (collapse, visibility)
import Common (shouldRenderFrom)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Test.Utils (describe)

spec :: forall m. MonadReader Int m => MonadAff m => m Unit
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
