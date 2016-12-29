module CSS.Display where

import Prelude
import CSS.Common (class Inherit, class None)
import CSS.Property (class Val, Value)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Generic (class Generic, gShow)

newtype Position = Position Value

derive instance eqPosition :: Eq Position
derive instance ordPosition :: Ord Position
derive instance genericPosition :: Generic Position

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

derive instance eqDisplay :: Eq Display
derive instance ordDisplay :: Ord Display
derive instance genericDisplay :: Generic Display

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

data Float = FloatLeft | FloatRight | FloatNone

derive instance eqFloat :: Eq Float
derive instance genericFloat :: Generic Float

instance showFloat :: Show Float where
  show = gShow

instance valFloat :: Val (Float) where
  value (FloatLeft) = fromString "left"
  value (FloatRight) = fromString "right"
  value (FloatNone) = fromString "none"

instance noneFloat :: None (Float) where
  none = FloatNone

floatLeft :: Float
floatLeft = FloatLeft

floatRight :: Float
floatRight = FloatRight

float :: Float -> CSS
float = key (fromString "float")

data ClearFloat
  = ClearFloatLeft
  | ClearFloatRight
  | ClearFloatBoth
  | ClearFloatNone
  | ClearFloatInherit
  | ClearFloatInlineStart
  | ClearFloatInlineEnd

derive instance eqClearFloat :: Eq ClearFloat
derive instance genericClearFloat :: Generic ClearFloat

instance showClearFloat :: Show ClearFloat where
  show = gShow

instance valClearFloat :: Val (ClearFloat) where
  value (ClearFloatLeft) = fromString "left"
  value (ClearFloatRight) = fromString "right"
  value (ClearFloatBoth) = fromString "both"
  value (ClearFloatNone) = fromString "none"
  value (ClearFloatInherit) = fromString "inherit"
  value (ClearFloatInlineStart) = fromString "inline-start"
  value (ClearFloatInlineEnd) = fromString "inline-end"

instance noneClearFloat :: None (ClearFloat) where
  none = ClearFloatNone

instance inheritClearFloat :: Inherit (ClearFloat) where
  inherit = ClearFloatInherit

clearLeft :: ClearFloat
clearLeft = ClearFloatLeft

clearRight :: ClearFloat
clearRight = ClearFloatRight

clearBoth :: ClearFloat
clearBoth = ClearFloatBoth

clearInlineStart :: ClearFloat
clearInlineStart = ClearFloatInlineStart

clearInlineEnd :: ClearFloat
clearInlineEnd = ClearFloatInlineEnd

clear :: ClearFloat -> CSS
clear = key (fromString "clear")
