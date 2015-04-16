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

main :: Eff (dom :: DOM) Unit
main = addStyleSheet <<< fromMaybe "" <<< renderedSheet $ render style
