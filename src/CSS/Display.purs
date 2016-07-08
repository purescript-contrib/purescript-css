module CSS.Display where

import Prelude
import CSS.Common (class Right, right, left, class Left, class None, class Inherit, class Initial, none, inherit, initial)
import CSS.Property (class Val, Value)
import CSS.Size (Size)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Generic (class Generic)

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

positionBottom :: forall a. Size a -> CSS
positionBottom v = key (fromString "bottom") $ v

positionLeft :: forall a. Size a -> CSS
positionLeft v = key (fromString "left") $ v

positionRight :: forall a. Size a -> CSS
positionRight v = key (fromString "right") $ v

positionTop :: forall a. Size a -> CSS
positionTop v = key (fromString "top") $ v

newtype Display = Display Value

derive instance eqDisplay :: Eq Display
derive instance ordDisplay :: Ord Display
derive instance genericDisplay :: Generic Display

instance valDisplay :: Val Display where
  value (Display v) = v

instance initialDisplay :: Initial Display where
  initial = Display initial

instance inheritDisplay :: Inherit Display where
  inherit = Display inherit

instance noneDisplay :: None Display where
  none = Display none

displayNone :: Display
displayNone = none

displayInherit :: Display
displayInherit = inherit

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

newtype Float = Float Value

derive instance eqFloat :: Eq Float
derive instance ordFloat :: Ord Float
derive instance genericFloat :: Generic Float

instance valFloat :: Val Float where
  value (Float v) = v

instance initialFloat :: Initial Float where
  initial = Float initial

instance inheritFloat :: Inherit Float where
  inherit = Float inherit

instance noneFloat :: None Float where
  none = Float none

instance leftFloat :: Left Float where
  left = Float left

instance rightFloat :: Right Float where
  right = Float right

float :: Float -> CSS
float = key $ fromString "float"

newtype Clear = Clear Value

derive instance eqClear :: Eq Clear
derive instance ordClear :: Ord Clear
derive instance genericClear :: Generic Clear

instance valClear :: Val Clear where
  value (Clear v) = v

instance initialClear :: Initial Clear where
  initial = Clear initial

instance inheritClear :: Inherit Clear where
  inherit = Clear inherit

instance noneClear :: None Clear where
  none = Clear none

instance leftClear :: Left Clear where
  left = Clear left

instance rightClear :: Right Clear where
  right = Clear right

both :: Clear
both = Clear $ fromString "both"
