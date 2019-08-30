module CSS.OverflowWrap where

import Prelude

import CSS.Common (class Inherit, class Initial, class Unset)
import CSS.Property (class Val, Value)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

data OverflowWrap = OverflowWrap Value

derive instance eqOverflowWrap :: Eq OverflowWrap
derive instance ordOverflowWrap :: Ord OverflowWrap

instance inheritOverflowWrap :: Inherit OverflowWrap where
  inherit = OverflowWrap $ fromString "inherit"

instance initialOverflowWrap :: Initial OverflowWrap where
  initial = OverflowWrap $ fromString "initial"

instance unsetOverflowWrap :: Unset OverflowWrap where
  unset = OverflowWrap $ fromString "unset"

instance valOverflowWrap :: Val OverflowWrap where
  value (OverflowWrap v) = v

overflowWrap :: OverflowWrap -> CSS
overflowWrap = key $ fromString "overflow-wrap"

normal :: OverflowWrap
normal = OverflowWrap $ fromString "normal"

anywhere :: OverflowWrap
anywhere = OverflowWrap $ fromString "anywhere"

breakWord :: OverflowWrap
breakWord = OverflowWrap $ fromString "break-word"

