module Css.Gradient where

import Css.Background
import Css.Color
import Css.Property
import Css.Size
import Css.String
import Data.Foldable

data ColorPoint = ColorPoint Color (Size Rel)

linearGradient :: forall a. Angle a -> ColorPoint -> [ColorPoint] -> ColorPoint-> BackgroundImage
linearGradient a b cs e = BackgroundImage $ fromString "linear-gradient(" <> value a <> fromString ", " <> points <> fromString ")"
  where colorPoint (ColorPoint a b) = value a <> fromString " " <> value b
        points = intercalate (fromString ", ") $ [colorPoint b] <> (colorPoint <$> cs) <> [colorPoint e]
