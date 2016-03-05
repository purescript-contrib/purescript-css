module Site where

import Prelude hiding (top)

import Data.List (List(..))
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
import Data.NonEmpty as NEL

foreign import addStyleSheet :: forall eff. String -> Eff (dom::DOM | eff) Unit
foreign import titleWidth    :: forall eff. Eff (dom::DOM | eff) Number
foreign import titleHeight   :: forall eff. Eff (dom::DOM | eff) Number
foreign import titleStyle    :: forall eff. String -> Eff (dom::DOM | eff) Unit

blue1 :: Color
blue1 = rgb 51 136 204

blue2 :: Color
blue2 = rgb 238 238 255

backgroundGradient :: forall a. Angle a -> CSS
backgroundGradient a = backgroundImage $ linearGradient a (uniformScale RGB white Nil blue2)

shake :: (Number -> Number) -> CSS
shake f = transforms [translate (px (f 3.0)) nil, rotate (deg (f 2.0))]

style :: CSS
style = do
  fontFace $ do
    fontFaceFamily $ fromString "Lato"
    fontWeight $ weight 300.0
    fontFaceSrc $ FontFaceSrcLocal "Lato Light" NEL.:|
                [ FontFaceSrcLocal "Lato-Light"
                , FontFaceSrcUrl "http://fonts.gstatic.com/s/lato/v11/EsvMC5un3kjyUhB9ZEPPwg.woff2" (Just WOFF2)
                ]

  keyframes "buzz-button" $ tuple2 50.0 (shake id) NEL.:| [tuple2 100.0 (shake negate)]

  query M.screen (NEL.singleton <<< M.maxWidth $ px 768.0) $
    h1 ? do
      fontSize (em 2.0)

  html ? height (pct 100.0)
  body ? do
    fontFamily ["Lato"] (NEL.singleton sansSerif)
    sym padding nil
    sym margin nil
    backgroundGradient (deg 0.0)
  (h1 ** a) ? do
    display block
    color blue1
    textDecoration noneTextDecoration
    fontWeight $ weight 100.0
    sym padding (em 0.5)
  h1 ? do
    fontSize (em 3.0)
    position absolute
    left (pct 50.0)
    top (pct 50.0)
    backgroundGradient (deg 180.0)
    border solid (px 1.0) blue1
    sym borderRadius (em 0.25)
  (h1 ## hover) ?
    animation (fromString "buzz-button") (sec 0.15) linear (sec 0.0) infinite normalAnimationDirection forwards

center :: Number -> Number -> CSS
center width height = do
  marginLeft (px $ -width / 2.0)
  marginTop (px $ -height / 2.0)


main :: Eff (dom :: DOM) Unit
main = do
  addStyleSheet <<< fromMaybe "" <<< renderedSheet $ render style
  width <- titleWidth
  height <- titleHeight
  titleStyle <<< fromMaybe "" <<< renderedInline <<< render $ center width height
