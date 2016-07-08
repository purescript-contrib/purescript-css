module Test.Main where

import Prelude
import CSS (Rendered, Path(..), Predicate(..), Refinement(..), Selector(..), renderedSheet, renderedInline, fromString, selector, block, display, render, borderBox, boxSizing, contentBox, blue, color, body, px, dashed, border, inlineBlock, red, (?))
import CSS.Common (none)
import CSS.List (inside, square, listStyleImage, listStylePosition, listStyleType)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Data.Maybe (Maybe(..))

example1 :: Rendered
example1 = render do
  color red
  display block

example2 :: Rendered
example2 = render do
  display inlineBlock

example3 :: Rendered
example3 = render do
  border dashed (px 2.0) blue

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

listStyle :: Rendered
listStyle = render do
  listStyleType square
  listStylePosition inside
  listStyleImage none

nestedNodes :: Rendered
nestedNodes = render do
  fromString "#parent" ? do
    display block
    fromString "#child" ? display block

nestedNodesWithEmptyParent :: Rendered
nestedNodesWithEmptyParent = render do
  fromString "#parent" ? do
    fromString "#child" ? display block

assertEqual :: forall a. (Eq a, Show a) => a -> a -> Eff (err :: EXCEPTION) Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y

main :: Eff (err :: EXCEPTION) Unit
main = do
  renderedInline example1 `assertEqual` Just "color: hsl(0.0, 100.0%, 50.0%); display: block"
  renderedInline example2 `assertEqual` Just "display: inline-block"
  renderedInline example3 `assertEqual` Just "border: dashed 2.0px hsl(240.0, 100.0%, 50.0%) "

  selector (Selector (Refinement [Id "test"]) Star) `assertEqual` "#test"

  selector (fromString "#test") `assertEqual` "#test"

  renderedSheet example4 `assertEqual` Just "body { color: hsl(240.0, 100.0%, 50.0%) }\n#world { display: block }\n"

  renderedInline example5 `assertEqual` Just "box-sizing: content-box; box-sizing: border-box"

  renderedInline listStyle `assertEqual` Just "list-style-type: square; list-style-position: inside; list-style-image: none"

  renderedSheet nestedNodes `assertEqual` Just "#parent { display: block }\n#parent #child { display: block }\n"

  renderedSheet nestedNodesWithEmptyParent `assertEqual` Just "#parent #child { display: block }\n"
