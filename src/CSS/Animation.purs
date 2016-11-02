module CSS.Animation where

import Prelude
import CSS.Property (class Val, Value, value)
import CSS.String (class IsString, fromString)
import CSS.Stylesheet (CSS, key)
import CSS.Time (Time)
import CSS.Transition (TimingFunction)
import Data.Foldable (for_)
import Data.Generic (class Generic)
import Data.Tuple.Nested (tuple7)

newtype AnimationDirection = AnimationDirection Value

derive instance eqAnimationDirection :: Eq AnimationDirection
derive instance ordAnimationDirection :: Ord AnimationDirection
derive instance genericAnimationDirection :: Generic AnimationDirection

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

derive instance eqIterationCount :: Eq IterationCount
derive instance ordIterationCount :: Ord IterationCount
derive instance genericIterationCount :: Generic IterationCount

instance valIterationCount :: Val IterationCount where
  value (IterationCount v) = v

infinite :: IterationCount
infinite = IterationCount $ fromString "infinite"

iterationCount :: Number -> IterationCount
iterationCount = IterationCount <<< value

newtype FillMode = FillMode Value

derive instance eqFillMode :: Eq FillMode
derive instance ordFillMode :: Ord FillMode
derive instance genericFillMode :: Generic FillMode

instance valFillMode :: Val FillMode where
  value (FillMode v) = v

forwards :: FillMode
forwards = FillMode $ fromString "forwards"

backwards :: FillMode
backwards = FillMode $ fromString "backwards"

animation :: AnimationName -> Time -> TimingFunction -> Time -> IterationCount -> AnimationDirection -> FillMode -> CSS
animation p de f du i di fm = do
  for_ animationKeys \k ->
    key (fromString k) (tuple7 p de f du i di fm)
  where
  animationKeys =
    [ "animation"
    , "-webkit-animation"
    , "-moz-animation"
    , "-o-animation"
    ]

newtype AnimationName = AnimationName Value

derive instance eqAnimationName :: Eq AnimationName
derive instance ordAnimationName :: Ord AnimationName
derive instance genericAnimationName :: Generic AnimationName

instance valAnimationName :: Val AnimationName where
  value (AnimationName v) = v

instance isStringAnimationName :: IsString AnimationName where
  fromString = AnimationName <<< fromString
