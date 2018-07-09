module CSS.Text.Overflow
  ( TextOverflow
  , textOverflow
  , clip
  , ellipsis
  , custom
  ) where

import CSS.Property (class Val, quote)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Ord (class Ord)

data TextOverflow
  = Clip
  | Ellipsis
  | Custom String

derive instance eqTextOverflow :: Eq TextOverflow
derive instance ordTextOverflow :: Ord TextOverflow

instance valTextOverflow :: Val TextOverflow where
  value Clip = fromString "clip"
  value Ellipsis = fromString "ellipsis"
  value (Custom v) = fromString $ quote v

textOverflow :: TextOverflow -> CSS
textOverflow = key $ fromString "text-overflow"

clip :: TextOverflow
clip = Clip

ellipsis :: TextOverflow
ellipsis = Ellipsis

custom :: String -> TextOverflow
custom = Custom
