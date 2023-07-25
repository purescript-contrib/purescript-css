module CSS.Pseudo where

import CSS.Selector (Refinement)
import CSS.String (fromString)

hover :: Refinement
hover = fromString ":hover"

before :: Refinement
before = fromString "::before"

after :: Refinement
after = fromString "::after"
