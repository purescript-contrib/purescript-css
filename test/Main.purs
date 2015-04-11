module Main where

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Css.Border
import Css.Color
import Css.Display
import Css.Elements
import Css.Font
import Css.Render
import Css.Size
import Css.String
import Css.Stylesheet
import Data.Maybe
import Data.These
import Debug.Trace

example1 :: Rendered
example1 = render do
  color red
  display block

example2 :: Rendered
example2 = render do
  display inlineBlock

example3 :: Rendered
example3 = render do
  border dashed (px 2) green

example4 :: Rendered
example4 = render do
  body ? do
    color green
  fromString "#world" ? do
    display block

inlineResult :: Rendered -> Maybe String
inlineResult (This (Inline a)) = Just a
inlineResult _ = Nothing

assertEqual :: forall a. (Eq a, Show a) => a -> a -> Eff (err :: Exception) Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y

main :: Eff (err :: Exception) Unit
main = do
  inlineResult example1 `assertEqual` Just "color: rgb(255, 0, 0); display: block"
  inlineResult example2 `assertEqual` Just "display: inline-block"
  inlineResult example3 `assertEqual` Just "border: dashed 2px rgb(0, 128, 0)"
