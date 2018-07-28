module CSS.Refinement where

import Prelude

import CSS.Predicate (Predicate(..))
import CSS.PseudoClass (PseudoClass(..))
import CSS.PseudoElement (PseudoElement(..))

newtype Refinement = Refinement (Array Predicate)

derive instance eqRefinement :: Eq Refinement
derive instance ordRefinement :: Ord Refinement

-- | Filter elements by id.
byId :: String -> Refinement
byId = Refinement <<< pure <<< Id

-- | Filter elements by class.
byClass :: String -> Refinement
byClass = Refinement <<< pure <<< Class

-- | Filter elements by pseudo selector functions.
-- | The preferred way is to use one of the predefined functions from `CSS.Pseudo`.
func :: String -> Array String -> Refinement
func f = Refinement <<< pure <<< PseudoFunc f

-- | Filter elements based on the presence of a certain attribute.
attr :: String -> Refinement
attr = Refinement <<< pure <<< Attr

-- | Filter elements based on the presence of a
-- | certain attribute with the specified value.
attrVal :: String -> String -> Refinement
attrVal a = Refinement <<< pure <<< AttrVal a
infix 6 attrVal as @=

-- | Filter elements based on the presence of a certain attribute that
-- | begins with the selected value.
attrBegins :: String -> String -> Refinement
attrBegins a = Refinement <<< pure <<< AttrBegins a
infix 6 attrBegins as ^=

-- | Filter elements based on the presence of a certain attribute that
-- | ends with the specified value.
attrEnds :: String -> String -> Refinement
attrEnds a = Refinement <<< pure <<< AttrEnds a
infix 6 attrEnds as $=

-- | Filter elements based on the presence of a certain attribute that contains
-- | the specified value as a substring.
attrContains :: String -> String -> Refinement
attrContains a = Refinement <<< pure <<< AttrContains a
infix 6 attrContains as *=

-- | Filter elements based on the presence of a certain attribute that
-- | have the specified value contained in a space separated list.
attrSpace :: String -> String -> Refinement
attrSpace a = Refinement <<< pure <<< AttrSpace a
infix 6 attrSpace as ~=

-- | Filter elements based on the presence of a certain attribute that
-- | have the specified value contained in a hyphen separated list.
attrHyph :: String -> String -> Refinement
attrHyph a = Refinement <<< pure <<< AttrHyph a
infix 6 attrHyph as |=

active :: Refinement
active = Refinement [PseudoClass Active]

any :: Refinement
any = Refinement [PseudoClass Any]

anyLink :: Refinement
anyLink = Refinement [PseudoClass AnyLink]

checked :: Refinement
checked = Refinement [PseudoClass Checked]

default :: Refinement
default = Refinement [PseudoClass Default]

defined :: Refinement
defined = Refinement [PseudoClass Defined]

disabled :: Refinement
disabled = Refinement [PseudoClass Disabled]

empty :: Refinement
empty = Refinement [PseudoClass Empty]

enabled :: Refinement
enabled = Refinement [PseudoClass Enabled]

first :: Refinement
first = Refinement [PseudoClass First]

firstChild :: Refinement
firstChild = Refinement [PseudoClass FirstChild]

firstOfType :: Refinement
firstOfType = Refinement [PseudoClass FirstOfType]

fullscreen :: Refinement
fullscreen = Refinement [PseudoClass Fullscreen]

focus :: Refinement
focus = Refinement [PseudoClass Focus]

focusVisible :: Refinement
focusVisible = Refinement [PseudoClass FocusVisible]

hover :: Refinement
hover = Refinement [PseudoClass Hover]

indeterminate :: Refinement
indeterminate = Refinement [PseudoClass Indeterminate]

inRange :: Refinement
inRange = Refinement [PseudoClass InRange]

invalid :: Refinement
invalid = Refinement [PseudoClass Invalid]

lastChild :: Refinement
lastChild = Refinement [PseudoClass LastChild]

lastOfType :: Refinement
lastOfType = Refinement [PseudoClass LastOfType]

left :: Refinement
left = Refinement [PseudoClass Left]

link :: Refinement
link = Refinement [PseudoClass Link]

onlyChild :: Refinement
onlyChild = Refinement [PseudoClass OnlyChild]

onlyOfType :: Refinement
onlyOfType = Refinement [PseudoClass OnlyOfType]

optional :: Refinement
optional = Refinement [PseudoClass Optional]

outOfRange :: Refinement
outOfRange = Refinement [PseudoClass OutOfRange]

readOnly :: Refinement
readOnly = Refinement [PseudoClass ReadOnly]

readWrite :: Refinement
readWrite = Refinement [PseudoClass ReadWrite]

required :: Refinement
required = Refinement [PseudoClass Required]

right :: Refinement
right = Refinement [PseudoClass Right]

root :: Refinement
root = Refinement [PseudoClass Root]

scope :: Refinement
scope = Refinement [PseudoClass Scope]

target :: Refinement
target = Refinement [PseudoClass Target]

valid :: Refinement
valid = Refinement [PseudoClass Valid]

visited :: Refinement
visited = Refinement [PseudoClass Visited]

firstLetter :: Refinement
firstLetter = Refinement [PseudoElement FirstLetter]

firstLine :: Refinement
firstLine = Refinement [PseudoElement FirstLine]

selection :: Refinement
selection = Refinement [PseudoElement Selection]

backdrop :: Refinement
backdrop = Refinement [PseudoElement Backdrop]

after :: Refinement
after = Refinement [PseudoElement After]

before :: Refinement
before = Refinement [PseudoElement Before]
