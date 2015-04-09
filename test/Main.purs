module Main where

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Css.Border
import Css.Display
import Css.Font
import Css.Render
import Css.Size
import Css.String
import Css.Stylesheet
import Css.Elements
import Data.These
import Data.Maybe
import Debug.Trace

import Data.Tuple

example1 :: Rendered
example1 = render do
  color $ fromString "red"
  display block

example2 :: Rendered
example2 = render do
  display inlineBlock

example3 :: Rendered
example3 = render do
  border dashed (px 2) (fromString "green")

example4 :: Rendered
example4 = render do
  body ? do
    color $ fromString "green"

inlineResult :: Rendered -> Maybe String
inlineResult (This (Inline a)) = Just a
inlineResult _ = Nothing

assertEqual :: forall a. (Eq a, Show a) => a -> a -> Eff (err :: Exception) Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y

main :: Eff (err :: Exception) Unit
main = do
  inlineResult example1 `assertEqual` Just "color: red; display: block"
  inlineResult example2 `assertEqual` Just "display: inline-block"
  inlineResult example3 `assertEqual` Just "border: dashed 2px green"
