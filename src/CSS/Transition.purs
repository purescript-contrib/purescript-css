module CSS.Transition where

import Prelude

import CSS.Time (Time)
import CSS.Common (browsers, none)
import CSS.String (fromString)
import CSS.Property (class Val, Value, value, (!))
import CSS.Stylesheet (CSS, prefixed, key)

data TimingStepsValue = Start | End

derive instance eqTimingStepsValue :: Eq TimingStepsValue
derive instance ordTimingStepsValue :: Ord TimingStepsValue

instance valTimingStepsValue :: Val TimingStepsValue where
  value Start = fromString "start"
  value End = fromString "end"

data TimingFunction
  = Ease
  | EaseIn
  | EaseOut
  | EaseInOut
  | Linear
  | StepStart
  | StepEnd
  | Steps Int TimingStepsValue
  | CubicBezier Number Number Number Number

derive instance eqTimingFunction :: Eq TimingFunction
derive instance ordTimingFunction :: Ord TimingFunction

instance valTimingFunction :: Val TimingFunction where
  value Ease = fromString "ease"
  value EaseIn = fromString "ease-in"
  value EaseOut = fromString "ease-out"
  value EaseInOut = fromString "ease-in-out"
  value Linear = fromString "linear"
  value StepStart = fromString "step-start"
  value StepEnd = fromString "step-end"
  value (Steps n v) = fromString "steps(" <> value [ fromString $ show n, value v ] <> fromString ")"
  value (CubicBezier a b c d) = fromString "cubic-bezier(" <> value [ a, b, c, d ] <> fromString ")"

ease :: TimingFunction
ease = Ease

easeIn :: TimingFunction
easeIn = EaseIn

easeOut :: TimingFunction
easeOut = EaseOut

easeInOut :: TimingFunction
easeInOut = EaseInOut

linear :: TimingFunction
linear = Linear

stepStart :: TimingFunction
stepStart = StepStart

stepEnd :: TimingFunction
stepEnd = StepEnd

steps :: Int -> TimingStepsValue -> TimingFunction
steps n = Steps n

cubicBezier :: Number -> Number -> Number -> Number -> TimingFunction
cubicBezier a b c d = CubicBezier a b c d

start :: TimingStepsValue
start = Start

end :: TimingStepsValue
end = End

transition :: String -> Time -> TimingFunction -> Time -> CSS
transition p d f e = prefixed (browsers <> fromString "transition") (p ! d ! f ! e)

transitionProperty :: String -> CSS
transitionProperty = key $ fromString "transition-property"

transitionProperties :: Array String -> CSS
transitionProperties [] = key (fromString "transition-property") (none :: Value)
transitionProperties xs = key (fromString "transition-property") xs

transitionDuration :: String -> CSS
transitionDuration = key $ fromString "transition-duration"

transitionDurations :: Array String -> CSS
transitionDurations [] = key (fromString "transition-duration") (none :: Value)
transitionDurations xs = key (fromString "transition-duration") xs

transitionTimingFunction :: TimingFunction -> CSS
transitionTimingFunction = key $ fromString "transition-timing-function"

transitionTimingFunctions :: Array TimingFunction -> CSS
transitionTimingFunctions [] = key (fromString "transition-timing-function") (none :: Value)
transitionTimingFunctions xs = key (fromString "transition-timing-function") xs

transitionDelay :: Time -> CSS
transitionDelay = key $ fromString "transition-delay"

transitionDelays :: Array Time -> CSS
transitionDelays = key $ fromString "transition-delay"
