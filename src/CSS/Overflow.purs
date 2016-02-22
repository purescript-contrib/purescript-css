module CSS.Overflow where

import Prelude

import CSS.Property
import CSS.String
import CSS.Stylesheet

newtype Overflow = Overflow Value

instance valOverflow :: Val Overflow where
  value (Overflow v) = v

overflow :: Overflow -> CSS
overflow = key $ fromString "overflow"

overflowX :: Overflow -> CSS
overflowX = key $ fromString "overflow-x"

overflowY :: Overflow -> CSS
overflowY = key $ fromString "overflow-y"

overflowAuto :: Overflow
overflowAuto = Overflow $ fromString "auto"

hidden :: Overflow
hidden = Overflow $ fromString "hidden"

scroll :: Overflow
scroll = Overflow $ fromString "scroll"

visible :: Overflow
visible = Overflow $ fromString "visible"

overflowInherit :: Overflow
overflowInherit = Overflow $ fromString "inherit"
