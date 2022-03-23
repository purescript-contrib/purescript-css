module Common where

import Prelude

import CSS (renderedInline)
import CSS.Render (render, renderedSheet)
import CSS.Color (Color, rgb)
import CSS.Stylesheet (CSS)
import Control.Alt ((<|>))
import Control.Monad.Reader.Class (class MonadReader)
import Effect.Aff.Class (class MonadAff)
import Test.Utils (it, shouldEqual)

shouldRenderFrom
  :: forall m
   . MonadReader Int m
  => MonadAff m
  => String
  -> CSS
  -> m Unit
shouldRenderFrom expected given =
  it ("renders " <> expected) $ expected `shouldRenderItFrom` given

shouldRenderItFrom
  :: forall m
   . MonadReader Int m
  => MonadAff m
  => String
  -> CSS
  -> m Unit
shouldRenderItFrom expected given =
  let
    r = render given
    actual = renderedInline r <|> renderedSheet r
  in
    actual `shouldEqual` pure expected

-- Colors below are originally from the X11 scheme:
-- https://github.com/purescript-contrib/purescript-colors/blob/b65b99478ce97adc819950de0316e46a941571dd/src/Color/Scheme/X11.purs
green :: Color
green = rgb 0 128 0

blue :: Color
blue = rgb 0 0 255

red :: Color
red = rgb 255 0 0

gold :: Color
gold = rgb 255 215 0

teal :: Color
teal = rgb 0 128 128

olive :: Color
olive = rgb 128 128 0
