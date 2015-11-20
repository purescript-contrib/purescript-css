module CSS.FontFace where

import Prelude

import Data.Maybe (Maybe(), maybe)
import Data.NonEmpty (NonEmpty())

import CSS.Property
import CSS.String
import CSS.Stylesheet

fontFaceFamily :: String -> CSS
fontFaceFamily = key (fromString "font-family") <<< Literal

data FontFaceFormat = WOFF
                    | WOFF2
                    | TrueType
                    | OpenType
                    | EmbeddedOpenType
                    | SVG

formatName :: FontFaceFormat -> String
formatName WOFF             = "woff"
formatName WOFF2            = "woff2"
formatName TrueType         = "truetype"
formatName OpenType         = "opentype"
formatName EmbeddedOpenType = "embedded-opentype"
formatName SVG              = "svg"

data FontFaceSrc = FontFaceSrcUrl String (Maybe FontFaceFormat)
                 | FontFaceSrcLocal String

instance valFontFaceSrc :: Val FontFaceSrc where
  value (FontFaceSrcUrl u f) = fromString $ "url(" <> quote u <> ")" <> maybe "" (\f' -> " format(" <> formatName f' <> ")") f
  value (FontFaceSrcLocal l) = fromString $ "local(" <> quote l <> ")"

fontFaceSrc :: NonEmpty Array FontFaceSrc -> CSS
fontFaceSrc = key $ fromString "src"
