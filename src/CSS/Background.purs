module CSS.Background where

import Prelude (($))

import CSS.Color (Color)
import CSS.Property (class Val, Value)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

backgroundColor :: Color -> CSS
backgroundColor = key $ fromString "background-color"

newtype BackgroundImage = BackgroundImage Value

instance valBackroundImage :: Val BackgroundImage where
  value (BackgroundImage v) = v

backgroundImage :: BackgroundImage -> CSS
backgroundImage = key $ fromString "background-image"
