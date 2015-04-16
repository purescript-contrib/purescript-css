module Site where

import Control.Monad.Eff
import Css.Color
import Css.Elements
import Css.Font
import Css.Render
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
style =
  body ? do
    fontFamily [] (NEL.singleton sansSerif)
    color green

main :: Eff (dom :: DOM) Unit
main = addStyleSheet <<< fromMaybe "" <<< renderedSheet $ render style
