module CSS.Cursor where

import Prelude

import CSS.Property (class Val)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

data Cursor
  = Default
  | Help
  | Pointer
  | Progress
  | Wait
  | Cell
  | Crosshair
  | Text
  | VerticalText
  | Alias
  | Copy
  | Move
  | NoDrop
  | NotAllowed
  | Grab
  | Grabbing
  | AllScroll
  | ColResize
  | RowResize
  | NResize
  | EResize
  | SResize
  | WResize
  | NEResize
  | NWResize
  | SEResize
  | SWResize
  | EWResize
  | NSResize
  | NESWResize
  | NWSEResize
  | ZoomIn
  | ZoomOut

derive instance eqCursor :: Eq Cursor
derive instance ordCursor :: Ord Cursor

instance valCursor :: Val Cursor where
  value Default = fromString "default"
  value Help = fromString "help"
  value Pointer = fromString "pointer"
  value Progress = fromString "progress"
  value Wait = fromString "wait"
  value Cell = fromString "cell"
  value Crosshair = fromString "crosshair"
  value Text = fromString "text"
  value VerticalText = fromString "vertical-text"
  value Alias = fromString "alias"
  value Copy = fromString "copy"
  value Move = fromString "move"
  value NoDrop = fromString "no-drop"
  value NotAllowed = fromString "not-allowed"
  value Grab = fromString "grab"
  value Grabbing = fromString "grabbing"
  value AllScroll = fromString "all-scroll"
  value ColResize = fromString "col-resize"
  value RowResize = fromString "row-resize"
  value NResize = fromString "n-resize"
  value EResize = fromString "e-resize"
  value SResize = fromString "s-resize"
  value WResize = fromString "w-resize"
  value NEResize = fromString "ne-resize"
  value NWResize = fromString "nw-resize"
  value SEResize = fromString "se-resize"
  value SWResize = fromString "sw-resize"
  value EWResize = fromString "ew-resize"
  value NSResize = fromString "ns-resize"
  value NESWResize = fromString "nesw-resize"
  value NWSEResize = fromString "nwse-resize"
  value ZoomIn = fromString "zoom-in"
  value ZoomOut = fromString "zoom-out"

cursor :: Cursor -> CSS
cursor = key $ fromString "cursor"

default :: Cursor
default = Default

help :: Cursor
help = Help

pointer :: Cursor
pointer = Pointer

progress :: Cursor
progress = Progress

wait :: Cursor
wait = Wait

cell :: Cursor
cell = Cell

crosshair :: Cursor
crosshair = Crosshair

text :: Cursor
text = Text

verticalText :: Cursor
verticalText = VerticalText

alias :: Cursor
alias = Alias

copy :: Cursor
copy = Copy

move :: Cursor
move = Move

noDrop :: Cursor
noDrop = NoDrop

notAllowed :: Cursor
notAllowed = NotAllowed

grab :: Cursor
grab = Grab

grabbing :: Cursor
grabbing = Grabbing

allScroll :: Cursor
allScroll = AllScroll

colResize :: Cursor
colResize = ColResize

rowResize :: Cursor
rowResize = RowResize

nResize :: Cursor
nResize = NResize

eResize :: Cursor
eResize = EResize

sResize :: Cursor
sResize = SResize

wResize :: Cursor
wResize = WResize

neResize :: Cursor
neResize = NEResize

nwResize :: Cursor
nwResize = NWResize

seResize :: Cursor
seResize = SEResize

swResize :: Cursor
swResize = SWResize

ewResize :: Cursor
ewResize = EWResize

nsResize :: Cursor
nsResize = NSResize

neswResize :: Cursor
neswResize = NESWResize

nwseResize :: Cursor
nwseResize = NWSEResize

zoomIn :: Cursor
zoomIn = ZoomIn

zoomOut :: Cursor
zoomOut = ZoomOut
