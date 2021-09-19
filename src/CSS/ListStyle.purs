module CSS.ListStyle where

import CSS.ListStyle.Image (ListStyleImage)
import CSS.ListStyle.Position (ListStylePosition)
import CSS.ListStyle.Type (ListStyleType)
import CSS.Property (class Val, value)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Tuple (Tuple(..))

data ListStyle = ListStyle ListStyleType ListStylePosition ListStyleImage

instance valueListStyle :: Val ListStyle where
  value (ListStyle t p i) = value (Tuple t (Tuple p i))

listStyle :: ListStyleType -> ListStylePosition -> ListStyleImage -> CSS
listStyle t p i = key (fromString "list-style") (ListStyle t p i)
