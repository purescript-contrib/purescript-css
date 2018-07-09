module CSS.Pseudo where

import CSS.Selector (Refinement)
import CSS.String (fromString)

hover :: Refinement
hover = fromString ":hover"

after :: Refinement
after = fromString "::after"

before :: Refinement
before = fromString "::before"

firstLetter :: Refinement
firstLetter = fromString "::first-letter"

firstLine :: Refinement
firstLine = fromString "::first-line"

selection :: Refinement
selection = fromString "::selection"

backdrop :: Refinement
backdrop = fromString "::backdrop"
