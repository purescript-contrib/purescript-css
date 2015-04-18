module Site where

import Control.Monad.Eff
import Css.Animation
import Css.Background
import Css.Border
import Css.Color
import Css.Display
import Css.Elements
import Css.Font
import Css.Geometry
import Css.Gradient
import Css.Pseudo
import Css.Render
import Css.Selector
import Css.Size
import Css.String
import Css.Stylesheet
import Css.Text
import Css.Time
import Css.Transform
import Css.Transition
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

backgroundGradient :: forall a. Angle a -> Css
backgroundGradient a = backgroundImage $ linearGradient a (ColorPoint white (pct 0)) [] (ColorPoint blue2 (pct 100))

shakeLeft :: Css
shakeLeft = transforms [translate (px (-3)) nil, rotate (deg (-2))]

shakeRight :: Css
shakeRight = transforms [translate (px 3) nil, rotate (deg 2)]

style :: Css
style = do
  keyframes "buzz-button" $ tuple2 50 shakeRight NEL.:| [tuple2 100 shakeLeft]

  html ? height (pct 100)
  body ? do
    fontFamily [] (NEL.singleton sansSerif)
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

center :: Number -> Number -> Css
center width height = do
  marginLeft (px $ -width / 2)
  marginTop (px $ -height / 2)

main :: Eff (dom :: DOM) Unit
main = do
  addStyleSheet <<< fromMaybe "" <<< renderedSheet $ render style

  width <- titleWidth
  height <- titleHeight
  titleStyle <<< fromMaybe "" <<< renderedInline <<< render $ center width height
