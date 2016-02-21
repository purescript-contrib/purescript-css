module CSS.Gradient where

import Prelude ((<>), ($), (<$>))

import Data.Foldable (intercalate)

import CSS.Background (BackgroundImage(BackgroundImage))
import CSS.Color (Color)
import CSS.Property (value)
import CSS.Size (Angle, Rel, Size)
import CSS.String (fromString)

data ColorPoint = ColorPoint Color (Size Rel)

linearGradient :: forall a. Angle a -> ColorPoint -> Array ColorPoint -> ColorPoint-> BackgroundImage
linearGradient a b cs e = BackgroundImage $ fromString "linear-gradient(" <> value a <> fromString ", " <> points <> fromString ")"
  where colorPoint (ColorPoint a b) = value a <> fromString " " <> value b
        points = intercalate (fromString ", ") $ [colorPoint b] <> (colorPoint <$> cs) <> [colorPoint e]
