module CSS.Transition where

import Prelude (($))

import CSS.String (fromString)
import CSS.Property (class Val, Value)

newtype TimingFunction = TimingFunction Value

instance valTimingFunction :: Val TimingFunction where
  value (TimingFunction v) = v

linear :: TimingFunction
linear = TimingFunction $ fromString "linear"

easeOut :: TimingFunction
easeOut = TimingFunction $ fromString "ease-out"
