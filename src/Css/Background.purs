module Css.Background where

import Css.Color
import Css.Property
import Css.String
import Css.Stylesheet

backgroundColor :: Color -> Css
backgroundColor = key $ fromString "background-color"

newtype BackgroundImage = BackgroundImage Value

instance valBackroundImage :: Val BackgroundImage where
  value (BackgroundImage v) = v

backgroundImage :: BackgroundImage -> Css
backgroundImage = key $ fromString "background-image"
