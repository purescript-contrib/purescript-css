module Main where

import Control.Monad.Eff
import Css.Display
import Css.String
import Css.Font
import Css.Render
import Debug.Trace
import Data.These

example :: Rendered
example = render do
  color $ fromString "red"
  display block

printResult :: Rendered -> Eff (trace :: Trace) Unit
printResult (This (Inline a)) = trace a
printResult (That (Sheet a))  = trace a
printResult (Both (Inline a) (Sheet b))  = do
  trace a
  trace b

main :: Eff (trace :: Trace) Unit
main = printResult example
