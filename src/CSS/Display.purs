module CSS.Display where

import Prelude (($))

import CSS.Property (class Val, Value)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

newtype Position = Position Value

instance valPosition :: Val Position where
  value (Position v) = v

position :: Position -> CSS
position = key $ fromString "position"

static :: Position
static = Position $ fromString "static"

absolute :: Position
absolute = Position $ fromString "absolute"

fixed :: Position
fixed = Position $ fromString "fixed"

relative :: Position
relative = Position $ fromString "relative"

newtype Display = Display Value

instance valDisplay :: Val Display where
  value (Display v) = v

inline :: Display
inline = Display $ fromString "inline"

block :: Display
block = Display $ fromString "block"

listItem :: Display
listItem = Display $ fromString "list-item"

runIn :: Display
runIn = Display $ fromString "runIn"

inlineBlock :: Display
inlineBlock = Display $ fromString "inline-block"

table :: Display
table = Display $ fromString "table"

inlineTable :: Display
inlineTable = Display $ fromString "inline-table"

tableRowGroup :: Display
tableRowGroup = Display $ fromString "table-row-Group"

tableHeaderGroup :: Display
tableHeaderGroup = Display $ fromString "table-header-group"

tableFooterGroup :: Display
tableFooterGroup = Display $ fromString "table-footer-group"

tableRow :: Display
tableRow = Display $ fromString "table-row"

tableColumnGroup :: Display
tableColumnGroup = Display $ fromString "table-column-group"

tableColumn :: Display
tableColumn = Display $ fromString "table-column"

tableCell :: Display
tableCell = Display $ fromString "table-cell"

tableCaption :: Display
tableCaption = Display $ fromString "table-caption"

displayNone :: Display
displayNone = Display $ fromString "none"

displayInherit :: Display
displayInherit = Display $ fromString "inherit"

flex :: Display
flex = Display $ fromString "flex"

inlineFlex :: Display
inlineFlex = Display $ fromString "inline-flex"

grid :: Display
grid = Display $ fromString "grid"

inlineGrid :: Display
inlineGrid = Display $ fromString "inline-grid"

display :: Display -> CSS
display = key $ fromString "display"
