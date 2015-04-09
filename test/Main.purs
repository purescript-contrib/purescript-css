module Main where

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Css.Display
import Css.Font
import Css.Render
import Css.String
import Data.These
import Data.Maybe
import Debug.Trace

example1 :: Rendered
example1 = render do
  color $ fromString "red"
  display block

example2 :: Rendered
example2 = render do
  display inlineBlock

inlineResult :: Rendered -> Maybe String
inlineResult (This (Inline a)) = Just a
inlineResult _ = Nothing

assertEqual :: forall a. (Eq a, Show a) => a -> a -> Eff (err :: Exception) Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y

main :: Eff (err :: Exception) Unit
main = do
  inlineResult example1 `assertEqual` Just "color: red; display: block"
  inlineResult example2 `assertEqual` Just "display: inline-block"
