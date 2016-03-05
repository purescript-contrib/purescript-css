module CSS.BoxSizing where

import Prelude (($))

import CSS.String (fromString)
import CSS.Property (Value(), Val)
import CSS.Stylesheet (CSS(), key)

newtype BoxSizing = BoxSizing Value

instance valBoxSizing :: Val BoxSizing where
    value (BoxSizing v) = v

boxSizing :: BoxSizing -> CSS
boxSizing = key $ fromString "box-sizing"

contentBox :: BoxSizing
contentBox = BoxSizing $ fromString "content-box"

borderBox :: BoxSizing
borderBox = BoxSizing $ fromString "border-box"
