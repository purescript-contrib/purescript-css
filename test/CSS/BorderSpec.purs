module CSS.BorderSpec where

import Prelude

import CSS.Border (dashed, dotted, double, groove, inset, outline, outlineColor, outlineOffset, outlineStyle, outlineWidth, ridge, solid)
import CSS.Color as Color
import CSS.Common (inherit, initial, unset)
import CSS.Size (em, px)
import Common (shouldRenderFrom, green, blue)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Maybe (fromJust)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Partial.Unsafe (unsafePartial)
import Test.Utils (describe)

spec :: forall m. MonadReader Int m => MonadAff m => m Unit
spec = do

  describe "outline (Mozilla examples)" $
    "outline: solid 3.0px hsl(120.0, 100.0%, 25.1%)" `shouldRenderFrom` outline solid (px 3.0) green

  describe "outline-style (Mozilla examples)" do
    let testOutlineStyle (s /\ v) = ("outline-style: " <> s) `shouldRenderFrom` outlineStyle v
    describe "Keyword values" $
      traverse_
        testOutlineStyle
        [ "dotted" /\ dotted
        , "dashed" /\ dashed
        , "solid" /\ solid
        , "double" /\ double
        , "groove" /\ groove
        , "ridge" /\ ridge
        , "inset" /\ inset
        ]
    describe "Global values" $
      traverse_
        testOutlineStyle
        [ "inherit" /\ inherit
        , "initial" /\ initial
        , "unset" /\ unset
        ]

  describe "outline-color (Mozilla examples)" $
    let
      testOutlineColor (s /\ v) = ("outline-color: " <> s) `shouldRenderFrom` outlineColor v
    in
      describe "<color> values" $
        traverse_
          testOutlineColor
          [ "hsl(0.0, 94.64%, 56.08%)" /\ (unsafePartial $ fromJust $ Color.fromHexString "#f92525")
          , "hsl(148.44, 76.19%, 49.41%)" /\ Color.rgb 30 222 121
          , "hsl(240.0, 100.0%, 50.0%)" /\ blue
          ]

  describe "outline-width (Mozilla examples)" do
    let testOutlineWidth (s /\ v) = ("outline-width: " <> s) `shouldRenderFrom` outlineWidth v
    describe "<length> values" $
      traverse_
        testOutlineWidth
        [ "1.0px" /\ px 1.0
        , "0.1em" /\ em 0.1
        ]
    describe "Global values" $
      traverse_
        testOutlineWidth
        [ "inherit" /\ inherit
        , "initial" /\ initial
        , "unset" /\ unset
        ]

  describe "outline-offset (Mozilla examples)" do
    let testOutlineOffset (s /\ v) = ("outline-offset: " <> s) `shouldRenderFrom` outlineOffset v
    describe "<length> values" $
      traverse_
        testOutlineOffset
        [ "3.0px" /\ px 3.0
        , "0.2em" /\ em 0.2
        ]
    describe "Global values" $
      traverse_
        testOutlineOffset
        [ "inherit" /\ inherit
        , "initial" /\ initial
        , "unset" /\ unset
        ]
