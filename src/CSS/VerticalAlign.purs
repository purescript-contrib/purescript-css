module CSS.VerticalAlign where

import CSS.Common (class Baseline, class Inherit, class Initial, class Unset, class Top, class Middle, class Bottom)
import CSS.Property (class Val)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)
import Data.Function (($))

data VerticalAlign
  = Baseline
  | Sub
  | Super
  | TextTop
  | TextBottom
  | Middle
  | Top
  | Bottom
  | Inherit
  | Initial
  | Unset

instance valVerticalAlign :: Val (VerticalAlign) where
  value (Baseline) = fromString "baseline"
  value (Sub) = fromString "sub"
  value (Super) = fromString "super"
  value (TextTop) = fromString "text-top"
  value (TextBottom) = fromString "text-bottom"
  value (Middle) = fromString "middle"
  value (Top) = fromString "top"
  value (Bottom) = fromString "bottom"
  value (Inherit) = fromString "inherit"
  value (Initial) = fromString "initial"
  value (Unset) = fromString "unset"

instance inheritVerticalAlign :: Inherit (VerticalAlign) where
  inherit = Inherit

instance initialVerticalAlign :: Initial (VerticalAlign) where
  initial = Initial

instance unsetVerticalAlign :: Unset (VerticalAlign) where
  unset = Unset

instance baselineVerticalAlign :: Baseline (VerticalAlign) where
  baseline = Baseline

instance middleVerticalAlign :: Middle (VerticalAlign) where
  middle = Middle

instance topVerticalAlign :: Top (VerticalAlign) where
  top = Top

instance bottomVerticalAlign :: Bottom (VerticalAlign) where
  bottom = Bottom

sub :: VerticalAlign
sub = Sub

super :: VerticalAlign
super = Super

textTop :: VerticalAlign
textTop = TextTop

textBottom :: VerticalAlign
textBottom = TextBottom

verticalAlign :: VerticalAlign -> CSS
verticalAlign = key $ fromString "vertical-align"
