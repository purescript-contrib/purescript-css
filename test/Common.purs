module Common where

import Prelude

import CSS (renderedInline)
import CSS.Render (render, renderedSheet)
import CSS.Stylesheet (CSS)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error)
import Test.Spec (SpecT, it)
import Test.Spec.Assertions (shouldEqual)

shouldRenderFrom
  :: forall g m
   . Monad m
  => MonadThrow Error g
  => String
  -> CSS
  -> SpecT g Unit m Unit
shouldRenderFrom expected given =
  it ("renders " <> expected) $ expected `shouldRenderItFrom` given

shouldRenderItFrom
  :: forall m
   . MonadThrow Error m
  => String
  -> CSS
  -> m Unit
shouldRenderItFrom expected given =
  let
    r = render given
    actual = renderedInline r <|> renderedSheet r
  in
    actual `shouldEqual` pure expected
