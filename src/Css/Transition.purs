module Css.Transition where

import Prelude

import Css.String
import Css.Property

newtype TimingFunction = TimingFunction Value

instance valTimingFunction :: Val TimingFunction where
  value (TimingFunction v) = v

linear :: TimingFunction
linear = TimingFunction $ fromString "linear"

easeOut :: TimingFunction
easeOut = TimingFunction $ fromString "ease-out"
