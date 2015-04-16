module Site where

import Control.Monad.Eff
import Css.Color
import Css.Display
import Css.Elements
import Css.Font
import Css.Geometry
import Css.Render
import Css.Size
import Css.Stylesheet
import Data.Maybe
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

style :: Css
style = do
  body ? do
    fontFamily [] (NEL.singleton sansSerif)
    padding nil nil nil nil
    margin nil nil nil nil
  h1 ? a ? color green
  h1 ? do
    position absolute
    left (pct 50)
    top (pct 50)

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
