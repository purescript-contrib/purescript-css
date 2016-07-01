module CSS.Overflow where

import Prelude

import Data.Generic (class Generic)

import CSS.Property (class Val, Value)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

newtype Overflow = Overflow Value

derive instance eqOverflow :: Eq Overflow
derive instance ordOverflow :: Ord Overflow
derive instance genericOverflow :: Generic Overflow

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
