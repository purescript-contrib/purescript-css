module CSS.Gradient
  ( module Color.Scale
  , linearGradient
  ) where

import Prelude

import CSS.Background (BackgroundImage(..))
import CSS.Property (value)
import CSS.Size (Angle)
import CSS.String (fromString)

import Color.Scale

linearGradient :: forall a. Angle a -> ColorScale -> BackgroundImage
linearGradient a cs = BackgroundImage $ fromString "linear-gradient(" <> value a <> fromString (", " <> cssColorStops cs <> ")")
