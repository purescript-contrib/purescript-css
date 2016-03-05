module Test.Main where

import Prelude
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import CSS.Border
import CSS.Color
import CSS.Display
import CSS.Elements
import CSS.Flexbox
import CSS.Font
import CSS.Render
import CSS.Selector
import CSS.Size
import CSS.String
import CSS.Stylesheet
import Data.Maybe

example1 :: Rendered
example1 = render do
  color red
  display block

example2 :: Rendered
example2 = render do
  display inlineBlock

example3 :: Rendered
example3 = render do
  border dashed (px 2.0) green

example4 :: Rendered
example4 = render do
  body ? do
    color green
  fromString "#world" ? do
    display block

example5 :: Rendered
example5 = render do
  flexDirection rowReverse
  flexWrap noWrap
  justifyContent flexStart
  alignItems baseline
  alignContent spaceBetween
  flexGrow 2
  flexBasis (pct 50.0)

assertEqual :: forall a. (Eq a, Show a) => a -> a -> Eff (err :: EXCEPTION) Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y

main :: Eff (err :: EXCEPTION) Unit
main = do
  renderedInline example1 `assertEqual` Just "color: rgb(255, 0, 0); display: block"
  renderedInline example2 `assertEqual` Just "display: inline-block"
  renderedInline example3 `assertEqual` Just "border: dashed 2.0px rgb(0, 128, 0)"

  selector (Selector (Refinement [Id "test"]) Star) `assertEqual` "#test"

  selector (fromString "#test") `assertEqual` "#test"

  renderedSheet example4 `assertEqual` Just "body { color: rgb(0, 128, 0) }\n#world { display: block }\n"

  renderedInline example5 `assertEqual` Just "flex-direction: row-reverse; flex-wrap: nowrap; justify-content: flex-start; align-items: baseline; align-content: space-between; flex-grow: 2; flex-basis: 50.0%"
