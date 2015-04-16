module Css.Background where

import Css.Property
import Css.String
import Css.Stylesheet

newtype BackgroundImage = BackgroundImage Value

instance valBackroundImage :: Val BackgroundImage where
  value (BackgroundImage v) = v

backgroundImage :: BackgroundImage -> Css
backgroundImage = key $ fromString "background-image"
