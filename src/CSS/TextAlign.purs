module CSS.TextAlign where

import Prelude

import CSS.Property
import CSS.String
import CSS.Stylesheet

newtype TextAlign = TextAlign Value

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
