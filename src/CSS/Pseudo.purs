module CSS.Pseudo where

import CSS.Selector (Refinement)
import CSS.String (fromString)

hover :: Refinement
hover = fromString ":hover"
