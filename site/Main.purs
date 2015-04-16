module Site where

import Control.Monad.Eff
import Css.Background
import Css.Border
import Css.Color
import Css.Display
import Css.Elements
import Css.Font
import Css.Geometry
import Css.Gradient
import Css.Render
import Css.Size
import Css.Stylesheet
import Css.Text
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

style :: Css
style = do
  html ? height (pct 100)
  body ? do
    fontFamily [] (NEL.singleton sansSerif)
    sym padding nil
    sym margin nil
    backgroundGradient (deg 0)
  h1 ? a ? do
    display block
    color blue1
    textDecoration noneTextDecoration
    fontWeight $ weight 100
    sym padding (em 0.5)
  h1 ? do
    position absolute
    left (pct 50)
    top (pct 50)
    backgroundGradient (deg 180)
    border solid (px 1) blue1

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
