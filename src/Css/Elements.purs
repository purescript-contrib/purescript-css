module Css.Elements where

import Prelude
import Css.Selector
import Css.String

body :: Selector
body = fromString "body"

a :: Selector
a = fromString "a"

h1 :: Selector
h1 = fromString "h1"

h2 :: Selector
h2 = fromString "h2"

h3 :: Selector
h3 = fromString "h3"

h4 :: Selector
h4 = fromString "h4"

h5 :: Selector
h5 = fromString "h5"

h6 :: Selector
h6 = fromString "h6"

html :: Selector
html = fromString "html"
