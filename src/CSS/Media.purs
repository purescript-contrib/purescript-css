module CSS.Media where

import Prelude

import Data.Maybe (Maybe(..))

import CSS.Property
import CSS.Size
import CSS.String
import CSS.Stylesheet

screen :: MediaType
screen = MediaType $ fromString "screen"

maxWidth :: Size Abs -> Feature
maxWidth = Feature "max-width" <<< Just <<< value
