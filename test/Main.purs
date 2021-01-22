module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Exception (error, throwException)
import CSS (Rendered, Path(..), Predicate(..), Refinement(..), Selector(..), FontFaceSrc(..), FontFaceFormat(..), pct, renderedSheet, renderedInline, fromString, selector, block, display, render, borderBox, boxSizing, contentBox, blue, color, body, a, p, px, dashed, border, inlineBlock, red, (?), (&), (|>), (|*), (|+), byId, byClass, (@=), (^=), ($=), (*=), (~=), (|=), hover, fontFaceSrc, fontStyle, deg, zIndex, textOverflow, opacity, cursor, transform, transition, easeInOut, cubicBezier, ms)
import CSS.Cursor as Cursor
import CSS.Flexbox (flex)
import CSS.FontStyle as FontStyle
import CSS.Text.Overflow as TextOverflow
import CSS.Transform as Transform
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton)

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

exampleCursor :: Rendered
exampleCursor = render do
  cursor Cursor.notAllowed

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

assertEqual :: forall a. Eq a => Show a => a -> a -> Effect Unit
assertEqual x y = unless (x == y) <<< throwException <<< error $ "Assertion failed: " <> show x <> " /= " <> show y

main :: Effect Unit
main = do
  renderedInline example1 `assertEqual` Just "color: hsl(0.0, 100.0%, 50.0%); display: block"
  renderedInline example2 `assertEqual` Just "display: inline-block"
  renderedInline example3 `assertEqual` Just "border: dashed 2.0px hsl(240.0, 100.0%, 50.0%) "

  selector (Selector (Refinement [Id "test"]) Star) `assertEqual` "#test"

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

  renderedInline exampleCursor `assertEqual` Just "cursor: not-allowed"

  renderedInline scaleTransform1 `assertEqual` Just "transform: scaleX(1.0); transform: scaleY(0.5); transform: scaleZ(0.5)"
  renderedInline scaleTransform2 `assertEqual` Just "transform: scale(0.2, 0.8)"

  renderedInline transition1 `assertEqual` Just "-webkit-transition: background-color 1.0ms ease-in-out 0.0ms; -moz-transition: background-color 1.0ms ease-in-out 0.0ms; -ms-transition: background-color 1.0ms ease-in-out 0.0ms; -o-transition: background-color 1.0ms ease-in-out 0.0ms; transition: background-color 1.0ms ease-in-out 0.0ms"
  renderedInline transition2 `assertEqual` Just "-webkit-transition: background-color 1.0ms cubic-bezier(0.3, 0.3, 0.7, 1.4) 0.0ms; -moz-transition: background-color 1.0ms cubic-bezier(0.3, 0.3, 0.7, 1.4) 0.0ms; -ms-transition: background-color 1.0ms cubic-bezier(0.3, 0.3, 0.7, 1.4) 0.0ms; -o-transition: background-color 1.0ms cubic-bezier(0.3, 0.3, 0.7, 1.4) 0.0ms; transition: background-color 1.0ms cubic-bezier(0.3, 0.3, 0.7, 1.4) 0.0ms"
