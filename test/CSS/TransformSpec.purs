module CSS.TransformSpec where

import Prelude

import CSS.Common (inherit, initial, none, unset)
import CSS.Transform (transform)
import Common (shouldRenderFrom)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Test.Utils (describe)

spec :: forall m. MonadReader Int m => MonadAff m => m Unit
spec = do
  describe "transform" do
    let testTransform (s /\ v) = ("transform: " <> s) `shouldRenderFrom` transform v
    describe "Global and keyword values" $
      traverse_
        testTransform
        [ "none" /\ none
        , "inherit" /\ inherit
        , "initial" /\ initial
        , "unset" /\ unset
        ]
