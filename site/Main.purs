module Site where

import Control.Monad.Eff
import CSS.Animation
import CSS.Background
import CSS.Border
import CSS.Color
import CSS.Display
import CSS.Elements
import CSS.Font
import CSS.FontFace
import CSS.Geometry
import CSS.Gradient
import qualified CSS.Media as M
import CSS.Pseudo
import CSS.Render
import CSS.Selector
import CSS.Size
import CSS.String
import CSS.Stylesheet
import CSS.Text
import CSS.Time
import CSS.Transform
import CSS.Transition
import Data.Maybe
import Data.Tuple.Nested
import DOM
import qualified Data.Array.NonEmpty as NEL

foreign import addStyleSheet """
  function addStyleSheet (s) {
    return function () {
      var e = document.createElement('style');
      e.appendChild(document.createTextNode(s));
      document.head.appendChild(e);
    };
  }
  """ :: String -> Eff (dom :: DOM) Unit

foreign import titleWidth """
  function titleWidth () {
    return document.getElementById('title').offsetWidth;
  }
  """ :: Eff (dom :: DOM) Number

foreign import titleHeight """
  function titleHeight () {
    return document.getElementById('title').offsetHeight;
  }
  """ :: Eff (dom :: DOM) Number

foreign import titleStyle """
  function titleStyle (s) {
    return function () {
      document.getElementById('title').setAttribute('style', s);
    };
  }
  """ :: String -> Eff (dom :: DOM) Unit

blue1 :: Color
blue1 = rgb 51 136 204

blue2 :: Color
blue2 = rgb 238 238 255

backgroundGradient :: forall a. Angle a -> CSS
backgroundGradient a = backgroundImage $ linearGradient a (ColorPoint white (pct 0)) [] (ColorPoint blue2 (pct 100))

shake :: (Number -> Number) -> CSS
shake f = transforms [translate (px (f 3)) nil, rotate (deg (f 2))]

style :: CSS
style = do
  fontFace $ do
    fontFaceFamily $ fromString "Lato"
    fontWeight $ weight 300
    fontFaceSrc $ FontFaceSrcLocal "Lato Light" NEL.:|
                [ FontFaceSrcLocal "Lato-Light"
                , FontFaceSrcUrl "http://fonts.gstatic.com/s/lato/v11/EsvMC5un3kjyUhB9ZEPPwg.woff2" (Just WOFF2)
                ]

  keyframes "buzz-button" $ tuple2 50 (shake id) NEL.:| [tuple2 100 (shake negate)]

  query M.screen (NEL.singleton <<< M.maxWidth $ px 768) $
    h1 ? do
      fontSize (em 2)

  html ? height (pct 100)
  body ? do
    fontFamily ["Lato"] (NEL.singleton sansSerif)
    sym padding nil
    sym margin nil
    backgroundGradient (deg 0)
  (h1 ** a) ? do
    display block
    color blue1
    textDecoration noneTextDecoration
    fontWeight $ weight 100
    sym padding (em 0.5)
  h1 ? do
    fontSize (em 3)
    position absolute
    left (pct 50)
    top (pct 50)
    backgroundGradient (deg 180)
    border solid (px 1) blue1
    sym borderRadius (em 0.25)
  (h1 ## hover) ?
    animation (fromString "buzz-button") (sec 0.15) linear (sec 0) infinite normalAnimationDirection forwards

center :: Number -> Number -> CSS
center width height = do
  marginLeft (px $ -width / 2)
  marginTop (px $ -height / 2)

main :: Eff (dom :: DOM) Unit
main = do
  addStyleSheet <<< fromMaybe "" <<< renderedSheet $ render style

  width <- titleWidth
  height <- titleHeight
  titleStyle <<< fromMaybe "" <<< renderedInline <<< render $ center width height
