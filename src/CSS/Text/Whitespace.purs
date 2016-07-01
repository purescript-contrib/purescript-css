module CSS.Text.Whitespace
  ( TextWhitespace
  , whitespaceNormal
  , whitespacePre
  , whitespaceNoWrap
  , whitespacePreWrap
  , whitespacePreLine
  , textWhitespace
  ) where

import Prelude

import Data.Generic (class Generic)

import CSS.Property (class Val, Value)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

newtype TextWhitespace = TextWhitespace Value

derive instance eqTextWhitespace :: Eq TextWhitespace
derive instance ordTextWhitespace :: Ord TextWhitespace
derive instance genericTextWhitespace :: Generic TextWhitespace

instance valTextWhitespace :: Val TextWhitespace where
  value (TextWhitespace v) = v

whitespaceNormal :: TextWhitespace
whitespaceNormal = TextWhitespace $ fromString "normal"

whitespacePre :: TextWhitespace
whitespacePre = TextWhitespace $ fromString "pre"

whitespaceNoWrap :: TextWhitespace
whitespaceNoWrap = TextWhitespace $ fromString "nowrap"

whitespacePreWrap :: TextWhitespace
whitespacePreWrap = TextWhitespace $ fromString "pre-wrap"

whitespacePreLine :: TextWhitespace
whitespacePreLine = TextWhitespace $ fromString "pre-line"

textWhitespace :: TextWhitespace -> CSS
textWhitespace = key $ fromString "white-space"
