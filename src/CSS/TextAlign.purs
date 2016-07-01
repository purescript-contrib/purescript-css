module CSS.TextAlign where

import Prelude

import Data.Generic (class Generic)

import CSS.Property (class Val, Value)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

newtype TextAlign = TextAlign Value

derive instance eqTextAlign :: Eq TextAlign
derive instance ordTextAlign:: Ord TextAlign
derive instance genericTextAlign :: Generic TextAlign

instance valTextAlign :: Val TextAlign where
  value (TextAlign v) = v

textAlign :: TextAlign -> CSS
textAlign = key $ fromString "text-align"

center :: TextAlign
center = TextAlign $ fromString "center"

justify :: TextAlign
justify = TextAlign $ fromString "justify"

leftTextAlign :: TextAlign
leftTextAlign = TextAlign $ fromString "left"

rightTextAlign :: TextAlign
rightTextAlign = TextAlign $ fromString "right"

inheritTextAlign :: TextAlign
inheritTextAlign = TextAlign $ fromString "inherit"

startTextAlign :: TextAlign
startTextAlign = TextAlign $ fromString "start"

endTextAlign :: TextAlign
endTextAlign = TextAlign $ fromString "end"
