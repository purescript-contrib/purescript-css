module CSS.Resize where

import Prelude

import CSS.Common (class Inherit, class Initial, class Unset, class None)
import CSS.Property (class Val, Value)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

data Resize = Resize Value

derive instance eqResize :: Eq Resize
derive instance ordResize :: Ord Resize

instance noneResize :: None Resize where
  none = Resize $ fromString "none"

instance inheritResize :: Inherit Resize where
  inherit = Resize $ fromString "inherit"

instance initialResize :: Initial Resize where
  initial = Resize $ fromString "initial"

instance unsetResize :: Unset Resize where
  unset = Resize $ fromString "unset"

instance valResize :: Val Resize where
  value (Resize v) = v

resize :: Resize -> CSS
resize = key $ fromString "resize"

both :: Resize
both = Resize $ fromString "both"

inlineResize :: Resize
inlineResize = Resize $ fromString "inline"

blockResize :: Resize
blockResize = Resize $ fromString "block"

horizontal :: Resize
horizontal = Resize $ fromString "horizontal"

vertical :: Resize
vertical = Resize $ fromString "vertical"

