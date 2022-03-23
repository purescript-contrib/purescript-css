module Test.Main where

import Prelude

import CSS (Rendered, Path(..), Predicate(..), Refinement(..), Selector(..), FontFaceSrc(..), FontFaceFormat(..), pct, renderedSheet, renderedInline, fromString, selector, block, display, render, borderBox, boxSizing, contentBox, color, body, a, p, px, dashed, border, inlineBlock, black, (?), (&), (|>), (|*), (|+), byId, byClass, (@=), (^=), ($=), (*=), (~=), (|=), hover, fontFaceSrc, fontStyle, deg, rgba, zIndex, textOverflow, opacity, cursor, transform, transition, easeInOut, cubicBezier, ms, direction, width, em, (@+@), (@-@), (@*), (*@), (@/))
import CSS.BorderSpec as BorderSpec
import CSS.DisplaySpec as DisplaySpec
import CSS.Cursor as Cursor
import CSS.Flexbox (flex)
import CSS.FontStyle as FontStyle
import CSS.Text.Direction as TextDirection
import CSS.Text.Overflow as TextOverflow
import CSS.Transform as Transform
import CSS.Common (none)
import CSS.Box (boxShadow, shadow, shadowWithBlur, shadowWithSpread, bsColor, bsInset)
import Common (blue, gold, red, teal, olive)
import Control.Monad.Reader (runReaderT)
import Control.Monad.RWS (modify_)
import Control.Monad.State (StateT, execStateT)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton, (:|))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throwException)

example1 :: Rendered
example1 = render do
  color red
  display block

example2 :: Rendered
example2 = render do
  display inlineBlock

example3 :: Rendered
example3 = render do
  border dashed (px 2.0) blue

example4 :: Rendered
example4 = render do
  body ? do
    color blue
  fromString "#world" ? do
    display block

example5 :: Rendered
example5 = render do
  boxSizing contentBox
  boxSizing borderBox

example6 :: Rendered
example6 = render do
  fontFaceSrc $ singleton $ FontFaceSrcUrl "font.woff" $ Just WOFF

example7 :: Rendered
example7 = render do
  zIndex 11
  opacity 0.5

example8 :: Rendered
example8 = render do
  flex 0.14 1.0 (pct 0.0)

withSelector :: Rendered
withSelector = render do
  a ? do
    color blue
  a & hover ? do
    color red

childSelector :: Rendered
childSelector = render do
  p |> a ? do
    zIndex 9

deepSelector :: Rendered
deepSelector = render do
  p |* a ? do
    display block

byClassById :: Rendered
byClassById = render do
  a & byClass "bar" ? color red
  p & byId "foo" ? display block

attrVal :: Rendered
attrVal = render do
  p & ("foo" @= "bar") ? display block

attrBegins :: Rendered
attrBegins = render do
  p & ("foo" ^= "bar") ? display block

attrEnds :: Rendered
attrEnds = render do
  p & ("foo" $= "bar") ? display block

attrContains :: Rendered
attrContains = render do
  p & ("foo" *= "bar") ? display block

attrSpace :: Rendered
attrSpace = render do
  p & ("foo" ~= "bar") ? display block

attrHyph :: Rendered
attrHyph = render do
  p & ("foo" |= "bar") ? display block

adjacentSelector :: Rendered
adjacentSelector = render do
  a |+ a ? do
    display inlineBlock

scaleTransform1 :: Rendered
scaleTransform1 = render do
  transform $ Transform.scaleX 1.0
  transform $ Transform.scaleY 0.5
  transform $ Transform.scaleZ 0.5

scaleTransform2 :: Rendered
scaleTransform2 = render do
  transform $ Transform.scale 0.2 0.8

exampleFontStyle1 :: Rendered
exampleFontStyle1 = render do
  fontStyle FontStyle.italic

exampleFontStyle2 :: Rendered
exampleFontStyle2 = render do
  fontStyle FontStyle.oblique

exampleFontStyle3 :: Rendered
exampleFontStyle3 = render do
  fontStyle $ FontStyle.obliqueAngle (deg 45.0)

exampleTextOverflow1 :: Rendered
exampleTextOverflow1 = render do
  textOverflow TextOverflow.ellipsis

exampleTextOverflow2 :: Rendered
exampleTextOverflow2 = render do
  textOverflow $ TextOverflow.custom "foobar"

exampleDirection :: Rendered
exampleDirection = render do
  direction TextDirection.rtl

exampleCursor :: Rendered
exampleCursor = render do
  cursor Cursor.notAllowed

noneShadow :: Rendered
noneShadow = render do
  boxShadow $ singleton $ none

singleShadow :: Rendered
singleShadow = render do
  boxShadow $ singleton $ bsColor teal $ shadow (px 60.0) (px (-16.0))

singleShadowWithBlur :: Rendered
singleShadowWithBlur = render do
  boxShadow $ singleton $ bsColor black $ shadowWithBlur (px 10.0) (px 5.0) (px 5.0)

singleShadowWithSpread :: Rendered
singleShadowWithSpread = render do
  boxShadow $ singleton $ rgba 0 0 0 0.2 `bsColor` shadowWithSpread (px 2.0) (px 2.0) (px 2.0) (px 1.0)

singleInsetShadow :: Rendered
singleInsetShadow = render do
  boxShadow $ singleton $ bsInset $ gold `bsColor` shadow (em 5.0) (em 1.0)

multipleShadows :: Rendered
multipleShadows = render do
  boxShadow $
    red `bsColor` shadow (px 3.0) (px 3.0) :|
      [ olive `bsColor` shadowWithBlur (em (-1.0)) (em 0.0) (em 0.4) ]

nestedNodes :: Rendered
nestedNodes = render do
  fromString "#parent" ? do
    display block
    fromString "#child" ? display block

nestedNodesWithEmptyParent :: Rendered
nestedNodesWithEmptyParent = render do
  fromString "#parent" ? do
    fromString "#child" ? display block

transition1 :: Rendered
transition1 = render do
  transition "background-color" (ms 1.0) easeInOut (ms 0.0)

transition2 :: Rendered
transition2 = render do
  transition "background-color" (ms 1.0) (cubicBezier 0.3 0.3 0.7 1.4) (ms 0.0)

calc1 :: Rendered
calc1 = render do
  width $ (em 2.0 @/ 3.0) @+@ px 1.0

calc2 :: Rendered
calc2 = render do
  width $ ((pct 100.0) @/ 7.0) @* 4.0

calc3 :: Rendered
calc3 = render do
  width $ 5.0 *@ (pct (-20.0) @-@ px 10.0)

assertEqual
  :: forall a
   . Eq a
  => Show a
  => a
  -> a
  -> StateT Int Effect Unit
assertEqual x y = do
  liftEffect $ unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y
  modify_ (_ + 1)

main :: Effect Unit
main = do
  count <- flip execStateT 0 do
    renderedInline example1 `assertEqual` Just "color: hsl(0.0, 100.0%, 50.0%); display: block"
    renderedInline example2 `assertEqual` Just "display: inline-block"
    renderedInline example3 `assertEqual` Just "border: dashed 2.0px hsl(240.0, 100.0%, 50.0%)"

    selector (Selector (Refinement [ Id "test" ]) Star) `assertEqual` "#test"

    selector (fromString "#test") `assertEqual` "#test"

    renderedSheet example4 `assertEqual` Just "body { color: hsl(240.0, 100.0%, 50.0%) }\n#world { display: block }\n"

    renderedInline example5 `assertEqual` Just "box-sizing: content-box; box-sizing: border-box"

    renderedSheet withSelector `assertEqual` Just "a { color: hsl(240.0, 100.0%, 50.0%) }\na:hover { color: hsl(0.0, 100.0%, 50.0%) }\n"
    renderedSheet childSelector `assertEqual` Just "p > a { z-index: 9 }\n"
    renderedSheet deepSelector `assertEqual` Just "p a { display: block }\n"
    renderedSheet adjacentSelector `assertEqual` Just "a + a { display: inline-block }\n"

    renderedSheet nestedNodes `assertEqual` Just "#parent { display: block }\n#parent #child { display: block }\n"

    renderedSheet nestedNodesWithEmptyParent `assertEqual` Just "#parent #child { display: block }\n"

    renderedInline example6 `assertEqual` Just "src: url(\"font.woff\") format(\"woff\")"

    renderedInline example7 `assertEqual` Just "z-index: 11; opacity: 0.5"

    renderedInline example8 `assertEqual` Just "flex: 0.14 1.0 0.0%"

    renderedInline exampleFontStyle1 `assertEqual` Just "font-style: italic"
    renderedInline exampleFontStyle2 `assertEqual` Just "font-style: oblique"
    renderedInline exampleFontStyle3 `assertEqual` Just "font-style: oblique 45.0deg"

    renderedInline exampleTextOverflow1 `assertEqual` Just "text-overflow: ellipsis"
    renderedInline exampleTextOverflow2 `assertEqual` Just "text-overflow: \"foobar\""

    renderedSheet byClassById `assertEqual` Just "a.bar { color: hsl(0.0, 100.0%, 50.0%) }\np#foo { display: block }\n"
    renderedSheet attrVal `assertEqual` Just "p[foo='bar'] { display: block }\n"
    renderedSheet attrBegins `assertEqual` Just "p[foo^='bar'] { display: block }\n"
    renderedSheet attrEnds `assertEqual` Just "p[foo$='bar'] { display: block }\n"
    renderedSheet attrContains `assertEqual` Just "p[foo*='bar'] { display: block }\n"
    renderedSheet attrSpace `assertEqual` Just "p[foo~='bar'] { display: block }\n"
    renderedSheet attrHyph `assertEqual` Just "p[foo|='bar'] { display: block }\n"

    renderedInline exampleDirection `assertEqual` Just "direction: rtl"

    renderedInline exampleCursor `assertEqual` Just "cursor: not-allowed"

    renderedInline scaleTransform1 `assertEqual` Just "transform: scaleX(1.0); transform: scaleY(0.5); transform: scaleZ(0.5)"
    renderedInline scaleTransform2 `assertEqual` Just "transform: scale(0.2, 0.8)"

    renderedInline transition1 `assertEqual` Just "-webkit-transition: background-color 1.0ms ease-in-out 0.0ms; -moz-transition: background-color 1.0ms ease-in-out 0.0ms; -ms-transition: background-color 1.0ms ease-in-out 0.0ms; -o-transition: background-color 1.0ms ease-in-out 0.0ms; transition: background-color 1.0ms ease-in-out 0.0ms"
    renderedInline transition2 `assertEqual` Just "-webkit-transition: background-color 1.0ms cubic-bezier(0.3, 0.3, 0.7, 1.4) 0.0ms; -moz-transition: background-color 1.0ms cubic-bezier(0.3, 0.3, 0.7, 1.4) 0.0ms; -ms-transition: background-color 1.0ms cubic-bezier(0.3, 0.3, 0.7, 1.4) 0.0ms; -o-transition: background-color 1.0ms cubic-bezier(0.3, 0.3, 0.7, 1.4) 0.0ms; transition: background-color 1.0ms cubic-bezier(0.3, 0.3, 0.7, 1.4) 0.0ms"

    renderedInline calc1 `assertEqual` Just "width: -webkit-calc((2.0em / 3.0) + 1.0px); width: -moz-calc((2.0em / 3.0) + 1.0px); width: -ms-calc((2.0em / 3.0) + 1.0px); width: -o-calc((2.0em / 3.0) + 1.0px); width: calc((2.0em / 3.0) + 1.0px)"
    renderedInline calc2 `assertEqual` Just "width: -webkit-calc(4.0 * (100.0% / 7.0)); width: -moz-calc(4.0 * (100.0% / 7.0)); width: -ms-calc(4.0 * (100.0% / 7.0)); width: -o-calc(4.0 * (100.0% / 7.0)); width: calc(4.0 * (100.0% / 7.0))"
    renderedInline calc3 `assertEqual` Just "width: -webkit-calc(5.0 * (-20.0% - 10.0px)); width: -moz-calc(5.0 * (-20.0% - 10.0px)); width: -ms-calc(5.0 * (-20.0% - 10.0px)); width: -o-calc(5.0 * (-20.0% - 10.0px)); width: calc(5.0 * (-20.0% - 10.0px))"

    renderedInline calc1 `assertEqual` Just "width: -webkit-calc((2.0em / 3.0) + 1.0px); width: -moz-calc((2.0em / 3.0) + 1.0px); width: -ms-calc((2.0em / 3.0) + 1.0px); width: -o-calc((2.0em / 3.0) + 1.0px); width: calc((2.0em / 3.0) + 1.0px)"
    renderedInline calc2 `assertEqual` Just "width: -webkit-calc(4.0 * (100.0% / 7.0)); width: -moz-calc(4.0 * (100.0% / 7.0)); width: -ms-calc(4.0 * (100.0% / 7.0)); width: -o-calc(4.0 * (100.0% / 7.0)); width: calc(4.0 * (100.0% / 7.0))"
    renderedInline calc3 `assertEqual` Just "width: -webkit-calc(5.0 * (-20.0% - 10.0px)); width: -moz-calc(5.0 * (-20.0% - 10.0px)); width: -ms-calc(5.0 * (-20.0% - 10.0px)); width: -o-calc(5.0 * (-20.0% - 10.0px)); width: calc(5.0 * (-20.0% - 10.0px))"

    renderedInline noneShadow `assertEqual` Just "-webkit-box-shadow: none; -moz-box-shadow: none; -ms-box-shadow: none; -o-box-shadow: none; box-shadow: none"
    renderedInline singleShadow `assertEqual` Just "-webkit-box-shadow: 60.0px -16.0px hsl(180.0, 100.0%, 25.1%); -moz-box-shadow: 60.0px -16.0px hsl(180.0, 100.0%, 25.1%); -ms-box-shadow: 60.0px -16.0px hsl(180.0, 100.0%, 25.1%); -o-box-shadow: 60.0px -16.0px hsl(180.0, 100.0%, 25.1%); box-shadow: 60.0px -16.0px hsl(180.0, 100.0%, 25.1%)"
    renderedInline singleShadowWithBlur `assertEqual` Just "-webkit-box-shadow: 10.0px 5.0px 5.0px hsl(0.0, 0.0%, 0.0%); -moz-box-shadow: 10.0px 5.0px 5.0px hsl(0.0, 0.0%, 0.0%); -ms-box-shadow: 10.0px 5.0px 5.0px hsl(0.0, 0.0%, 0.0%); -o-box-shadow: 10.0px 5.0px 5.0px hsl(0.0, 0.0%, 0.0%); box-shadow: 10.0px 5.0px 5.0px hsl(0.0, 0.0%, 0.0%)"
    renderedInline singleShadowWithSpread `assertEqual` Just "-webkit-box-shadow: 2.0px 2.0px 2.0px 1.0px hsla(0.0, 0.0%, 0.0%, 0.2); -moz-box-shadow: 2.0px 2.0px 2.0px 1.0px hsla(0.0, 0.0%, 0.0%, 0.2); -ms-box-shadow: 2.0px 2.0px 2.0px 1.0px hsla(0.0, 0.0%, 0.0%, 0.2); -o-box-shadow: 2.0px 2.0px 2.0px 1.0px hsla(0.0, 0.0%, 0.0%, 0.2); box-shadow: 2.0px 2.0px 2.0px 1.0px hsla(0.0, 0.0%, 0.0%, 0.2)"
    renderedInline multipleShadows `assertEqual` Just "-webkit-box-shadow: 3.0px 3.0px hsl(0.0, 100.0%, 50.0%), -1.0em 0.0em 0.4em hsl(60.0, 100.0%, 25.1%); -moz-box-shadow: 3.0px 3.0px hsl(0.0, 100.0%, 50.0%), -1.0em 0.0em 0.4em hsl(60.0, 100.0%, 25.1%); -ms-box-shadow: 3.0px 3.0px hsl(0.0, 100.0%, 50.0%), -1.0em 0.0em 0.4em hsl(60.0, 100.0%, 25.1%); -o-box-shadow: 3.0px 3.0px hsl(0.0, 100.0%, 50.0%), -1.0em 0.0em 0.4em hsl(60.0, 100.0%, 25.1%); box-shadow: 3.0px 3.0px hsl(0.0, 100.0%, 50.0%), -1.0em 0.0em 0.4em hsl(60.0, 100.0%, 25.1%)"

  log $ "\x1b[32m" <> show count <> " test" <> if count == 1 then "" else "s" <> " passed. These will be migrated to the new format in the future.\x1b[0m\n"

  launchAff_ $
    flip runReaderT 0 do
      BorderSpec.spec
      DisplaySpec.spec
