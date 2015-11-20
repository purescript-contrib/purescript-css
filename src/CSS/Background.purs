module CSS.Background where

import Prelude

import CSS.Color
import CSS.Property
import CSS.String
import CSS.Stylesheet

backgroundColor :: Color -> CSS
backgroundColor = key $ fromString "background-color"

newtype BackgroundImage = BackgroundImage Value

instance valBackroundImage :: Val BackgroundImage where
  value (BackgroundImage v) = v

backgroundImage :: BackgroundImage -> CSS
backgroundImage = key $ fromString "background-image"
