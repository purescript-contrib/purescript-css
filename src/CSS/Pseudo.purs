module CSS.Pseudo where

import CSS.Selector (Refinement)
import CSS.String (fromString)

active :: Refinement
active = fromString ":active"

any :: Refinement
any = fromString ":any"

anyLink :: Refinement
anyLink = fromString ":any-link"

checked :: Refinement
checked = fromString ":checked"

default :: Refinement
default = fromString ":default"

defined :: Refinement
defined = fromString ":defined"

-- TODO: dir

disabled :: Refinement
disabled = fromString ":disabled"

empty :: Refinement
empty = fromString ":empty"

enabled :: Refinement
enabled = fromString ":enabled"

first :: Refinement
first = fromString ":first"

firstChild :: Refinement
firstChild = fromString ":first-child"

firstOfType :: Refinement
firstOfType = fromString ":first-of-type"

fullscreen :: Refinement
fullscreen = fromString ":fullscreen"

focus :: Refinement
focus = fromString ":focus"

focusVisible :: Refinement
focusVisible = fromString ":focus-visible"

-- TODO: host, host-context

hover :: Refinement
hover = fromString ":hover"

indeterminate :: Refinement
indeterminate = fromString ":indeterminate"

inRange :: Refinement
inRange = fromString ":in-range"

invalid :: Refinement
invalid = fromString ":invalid"

-- TODO: lang

lastChild :: Refinement
lastChild = fromString ":last-child"

lastOfType :: Refinement
lastOfType = fromString ":last-of-type"

left :: Refinement
left = fromString ":left"

link :: Refinement
link = fromString ":link"

-- TODO: not, nth-child, nth-last-child
-- TODO: nth-last-of-type, nth-of-type

onlyChild :: Refinement
onlyChild = fromString ":only-child"

onlyOfType :: Refinement
onlyOfType = fromString ":only-of-type"

optional :: Refinement
optional = fromString ":optional"

outOfRange :: Refinement
outOfRange = fromString ":out-of-range"

readOnly :: Refinement
readOnly = fromString ":read-only"

readWrite :: Refinement
readWrite = fromString ":read-write"

required :: Refinement
required = fromString ":required"

right :: Refinement
right = fromString ":right"

root :: Refinement
root = fromString ":root"

scope :: Refinement
scope = fromString ":scope"

target :: Refinement
target = fromString ":target"

valid :: Refinement
valid = fromString ":valid"

visited :: Refinement
visited = fromString ":visited"
