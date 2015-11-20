module CSS.Animation where

import Prelude

import Data.Tuple.Nested (tuple7)

import CSS.Property
import CSS.String
import CSS.Stylesheet
import CSS.Time
import CSS.Transition

newtype AnimationDirection = AnimationDirection Value

instance valAnimationDirection :: Val AnimationDirection where
  value (AnimationDirection v) = v

normalAnimationDirection :: AnimationDirection
normalAnimationDirection = AnimationDirection $ fromString "normal"

alternate :: AnimationDirection
alternate = AnimationDirection $ fromString "alternate"

reverse :: AnimationDirection
reverse = AnimationDirection $ fromString "reverse"

alternateReverse :: AnimationDirection
alternateReverse = AnimationDirection $ fromString "alternate-reverse"

newtype IterationCount = IterationCount Value

instance valIterationCount :: Val IterationCount where
  value (IterationCount v) = v

infinite :: IterationCount
infinite = IterationCount $ fromString "infinite"

iterationCount :: Number -> IterationCount
iterationCount = IterationCount <<< value

newtype FillMode = FillMode Value

instance valFillMode :: Val FillMode where
  value (FillMode v) = v

forwards :: FillMode
forwards = FillMode $ fromString "forwards"

backwards :: FillMode
backwards = FillMode $ fromString "backwards"

animation :: AnimationName -> Time -> TimingFunction -> Time -> IterationCount -> AnimationDirection -> FillMode -> CSS
animation p de f du i di fm = key (fromString "-webkit-animation") (tuple7 p de f du i di fm)

newtype AnimationName = AnimationName Value

instance valAnimationName :: Val AnimationName where
  value (AnimationName v) = v

instance isStringAnimationName :: IsString AnimationName where
  fromString = AnimationName <<< fromString
