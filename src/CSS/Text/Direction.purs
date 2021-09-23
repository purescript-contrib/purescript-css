module CSS.Text.Direction where

import Prelude
import CSS.Property (class Val)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

data TextDirection = Ltr | Rtl

derive instance eqTextDirection :: Eq TextDirection
derive instance ordTextDirection :: Ord TextDirection

instance valTextDirection :: Val TextDirection where
  value Ltr = fromString "ltr"
  value Rtl = fromString "rtl"

direction :: TextDirection -> CSS
direction = key $ fromString "direction"

ltr :: TextDirection
ltr = Ltr

rtl :: TextDirection
rtl = Rtl
