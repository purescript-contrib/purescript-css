module CSS.Box
  ( BoxType
  , paddingBox, borderBox, contentBox
  , boxSizing
  , boxShadow
  , insetBoxShadow
  )
where

import Prelude

import Data.Generic (class Generic)

import CSS.Border (Stroke)
import CSS.Color (Color)
import CSS.Common (class Inherit, browsers)
import CSS.Property (class Val, Value, (!))
import CSS.Size (Size)
import CSS.String (class IsString, fromString)
import CSS.Stylesheet (CSS, prefixed, key)

-------------------------------------------------------------------------------

newtype BoxType = BoxType Value

derive instance eqBoxType :: Eq BoxType
derive instance ordBoxType :: Ord BoxType
derive instance genericBoxType :: Generic BoxType

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

boxShadow :: forall a. Size a -> Size a -> Size a -> Color -> CSS
boxShadow x y w c =
  prefixed (browsers <> fromString "box-shadow") (x ! y ! w ! c)

-------------------------------------------------------------------------------

insetBoxShadow ::
  forall a. Stroke -> Size a -> Size a -> Size a -> Color -> CSS
insetBoxShadow x y w c z = prefixed (browsers <> fromString "box-shadow") (x ! y ! w ! c ! z)
