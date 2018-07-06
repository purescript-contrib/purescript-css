module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Exception (error, throwException)
import CSS (Rendered, Path(..), Predicate(..), Refinement(..), Selector(..), FontFaceSrc(..), FontFaceFormat(..), renderedSheet, renderedInline, fromString, selector, block, display, render, borderBox, boxSizing, contentBox, blue, violet, color, body, px, dashed, solid, border, outline, inlineBlock, red, (?), fontFaceSrc, zIndex)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton)

example1 :: Rendered
example1 = render do
  color red
  display block

example2 :: Rendered
example2 = render do
  display inlineBlock

example3a :: Rendered
example3a = render do
  border dashed (px 2.0) blue

example3b :: Rendered
example3b = render do
  outline solid (px 1.0) violet

example4 :: Rendered
example4 = render do
  body ? do
    color blue
  fromString "#world" ? do
    display block

example5 :: Rendered
example5 = render do
  boxSizing contentBox
  boxSizing borderBox

example6 :: Rendered
example6 = render do
  fontFaceSrc $ singleton $ FontFaceSrcUrl "font.woff" $ Just WOFF

example7 :: Rendered
example7 = render do
  zIndex 11

nestedNodes :: Rendered
nestedNodes = render do
  fromString "#parent" ? do
    display block
    fromString "#child" ? display block

nestedNodesWithEmptyParent :: Rendered
nestedNodesWithEmptyParent = render do
  fromString "#parent" ? do
    fromString "#child" ? display block

assertEqual :: forall a. Eq a => Show a => a -> a -> Effect Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y

main :: Effect Unit
main = do
  renderedInline example1 `assertEqual` Just "color: hsl(0.0, 100.0%, 50.0%); display: block"
  renderedInline example2 `assertEqual` Just "display: inline-block"
  renderedInline example3a `assertEqual` Just "border: dashed 2.0px hsl(240.0, 100.0%, 50.0%) "
  renderedInline example3b `assertEqual` Just "outline: solid 1.0px hsl(300.0, 76.06%, 72.16%) "

  selector (Selector (Refinement [Id "test"]) Star) `assertEqual` "#test"

  selector (fromString "#test") `assertEqual` "#test"

  renderedSheet example4 `assertEqual` Just "body { color: hsl(240.0, 100.0%, 50.0%) }\n#world { display: block }\n"

  renderedInline example5 `assertEqual` Just "box-sizing: content-box; box-sizing: border-box"

  renderedSheet nestedNodes `assertEqual` Just "#parent { display: block }\n#parent #child { display: block }\n"

  renderedSheet nestedNodesWithEmptyParent `assertEqual` Just "#parent #child { display: block }\n"

  renderedInline example6 `assertEqual` Just "src: url(\"font.woff\") format(\"woff\")"

  renderedInline example7 `assertEqual` Just "z-index: 11"
