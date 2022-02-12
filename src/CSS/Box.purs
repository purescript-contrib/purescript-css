module CSS.Box
  ( BoxType
  , BoxShadow
  , paddingBox
  , borderBox
  , contentBox
  , boxSizing
  , boxShadow
  , shadow
  , shadowWithBlur
  , shadowWithSpread
  , bsInset
  , bsColor
  ) where

import Prelude

import Data.NonEmpty (NonEmpty)
import CSS.Color (Color)
import CSS.Common (class None, class Inherit, class Initial, class Unset, browsers)
import CSS.Property (class Val, Value, value, (!))
import CSS.Size (Size)
import CSS.String (class IsString, fromString)
import CSS.Stylesheet (CSS, prefixed, key)

-------------------------------------------------------------------------------

newtype BoxType = BoxType Value

derive instance eqBoxType :: Eq BoxType
derive instance ordBoxType :: Ord BoxType

instance isStringBoxType :: IsString BoxType where
  fromString = BoxType <<< fromString

instance valBoxType :: Val BoxType where
  value (BoxType v) = v

instance inheritBoxType :: Inherit BoxType where
  inherit = fromString "inherit"

-- | *DEPRECATED*
paddingBox :: BoxType
paddingBox = BoxType $ fromString "padding-box"

borderBox :: BoxType
borderBox = BoxType $ fromString "border-box"

contentBox :: BoxType
contentBox = BoxType $ fromString "content-box"

-------------------------------------------------------------------------------

boxSizing :: BoxType -> CSS
boxSizing = key $ fromString "box-sizing"

-------------------------------------------------------------------------------

newtype BoxShadow = BoxShadow Value

derive instance eqBoxShadow :: Eq BoxShadow
derive instance ordBoxShadow :: Ord BoxShadow

instance valBoxShadow :: Val BoxShadow where
  value (BoxShadow v) = v

instance isStringBoxShadow :: IsString BoxShadow where
  fromString = BoxShadow <<< fromString

instance noneBoxShadow :: None BoxShadow where
  none = fromString "none"

instance inheritBoxShadow :: Inherit BoxShadow where
  inherit = fromString "inherit"

instance initialBoxShadow :: Initial BoxShadow where
  initial = fromString "initial"

instance unsetBoxShadow :: Unset BoxShadow where
  unset = fromString "unset"

-- | This function will usually take a singleton list, but requiring a (non-empty) list
-- | prevents accidentally applying the modifiers (`bsInset`, `bsColor`) incorrectly.
-- |
-- | `singleton` (from "Data.NonEmpty") creates a singleton list.
-- |
-- | ```purescript
-- | boxShadow $ singleton $ shadow (px 1.0) (px 1.0)
-- | ```
-- |
-- | For supplying multiple `BoxShadow` values:
-- |
-- | ```purescript
-- | boxShadow $
-- |   red `bsColor` shadow (px 3.0) (px 3.0) :|
-- |   [olive `bsColor` shadowWithBlur (em (-1.0)) (em 0.0) (em 0.4)]
-- | ```
boxShadow :: NonEmpty Array BoxShadow -> CSS
boxShadow = prefixed (browsers <> fromString "box-shadow") <<< value

shadow :: forall a. Size a -> Size a -> BoxShadow
shadow x y = BoxShadow <<< value $ (x ! y)

shadowWithBlur :: forall a. Size a -> Size a -> Size a -> BoxShadow
shadowWithBlur x y w = BoxShadow <<< value $ (x ! y ! w)

shadowWithSpread :: forall a. Size a -> Size a -> Size a -> Size a -> BoxShadow
shadowWithSpread x y blurRadius spreadRadius =
  BoxShadow <<< value $ (x ! y ! blurRadius ! spreadRadius)

-- | Adapt the provided `box-shadow` with the `inset` prefix.
bsInset :: BoxShadow -> BoxShadow
bsInset (BoxShadow v) = BoxShadow <<< value $ ("inset" ! v)

-- | Supply a color to the provided `box-shadow`.
bsColor :: Color -> BoxShadow -> BoxShadow
bsColor c (BoxShadow v) = BoxShadow <<< value $ (v ! c)
