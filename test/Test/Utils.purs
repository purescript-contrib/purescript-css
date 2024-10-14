module Test.Utils where

import Prelude

import Control.Monad.Reader.Class (class MonadReader, ask, local)
import Data.Monoid (power, guard)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff)
import Test.Assert (assertEqual)

-----------------------------------------------------------------

-- Provide similar API to purescript-spec to reduce code changes

describe :: forall m. MonadReader Int m => MonadAff m => String -> m Unit -> m Unit
describe msg runTest = do
  indentation <- ask
  let spacing = guard (indentation > 0) " "
  liftEffect $ log $ (power ">>" indentation) <> spacing <> msg
  local (_ + 1) runTest

it :: forall m. MonadReader Int m => MonadAff m => String -> m Unit -> m Unit
it = describe

shouldEqual :: forall m a. MonadAff m => Eq a => Show a => a -> a -> m Unit
shouldEqual actual expected =
  liftEffect $ assertEqual { actual, expected }
