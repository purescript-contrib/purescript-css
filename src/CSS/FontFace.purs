module CSS.FontFace where

import Prelude

import Data.Generic (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.NonEmpty (NonEmpty)

import CSS.Property (class Val, Literal(..), quote)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

fontFaceFamily :: String -> CSS
fontFaceFamily = key (fromString "font-family") <<< Literal

data FontFaceFormat
  = WOFF
  | WOFF2
  | TrueType
  | OpenType
  | EmbeddedOpenType
  | SVG

derive instance eqFontFaceFormat :: Eq FontFaceFormat
derive instance ordFontFaceFormat :: Ord FontFaceFormat
derive instance genericFontFaceFormat :: Generic FontFaceFormat

formatName :: FontFaceFormat -> String
formatName WOFF = "woff"
formatName WOFF2 = "woff2"
formatName TrueType = "truetype"
formatName OpenType = "opentype"
formatName EmbeddedOpenType = "embedded-opentype"
formatName SVG = "svg"

data FontFaceSrc
  = FontFaceSrcUrl String (Maybe FontFaceFormat)
  | FontFaceSrcLocal String

derive instance eqFontFaceSrc :: Eq FontFaceSrc
derive instance ordFontFaceSrc :: Ord FontFaceSrc
derive instance genericFontFaceSrc :: Generic FontFaceSrc

instance valFontFaceSrc :: Val FontFaceSrc where
  value (FontFaceSrcUrl u f) = fromString $ "url(" <> quote u <> ")" <> maybe "" (\f' -> " format(" <> quote (formatName f') <> ")") f
  value (FontFaceSrcLocal l) = fromString $ "local(" <> quote l <> ")"

fontFaceSrc :: NonEmpty Array FontFaceSrc -> CSS
fontFaceSrc = key $ fromString "src"
