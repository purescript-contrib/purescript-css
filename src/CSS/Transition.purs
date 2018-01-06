module CSS.Transition where

import Prelude

import Data.Generic  (class Generic)
import Data.Foldable (intercalate)

import CSS.String   (fromString)
import CSS.Property (class Val, Value)

newtype TimingFunction = TimingFunction Value

derive instance eqTimingFunction      :: Eq TimingFunction
derive instance ordTimingFunction     :: Ord TimingFunction
derive instance genericTimingFunction :: Generic TimingFunction

instance valTimingFunction :: Val TimingFunction where
  value (TimingFunction v) = v

data StepEnum = Start
              | End

-- Specifies an animation with the same speed from start to end
linear :: TimingFunction
linear = TimingFunction $ fromString "linear"

-- Specifies an animation with a slow start, then fast, then end slowly (this is default)
ease :: TimingFunction
ease = TimingFunction $ fromString "ease"

-- Specifies an animation with a slow start
easeIn :: TimingFunction
easeIn = TimingFunction $ fromString "ease-in"

-- Specifies an animation with a slow end
easeOut :: TimingFunction
easeOut = TimingFunction $ fromString "ease-out"

-- Specifies an animation with a slow start and end
easeInOut :: TimingFunction
easeInOut = TimingFunction $ fromString "ease-in-out"

-- Equivalent to steps(1, start)
stepStart :: TimingFunction
stepStart = steps' 1 Start

-- Equivalent to steps(1, end)
stepEnd :: TimingFunction
stepEnd = steps' 1 End

-- Specifies a stepping function, with two parameters. The first parameter
-- specifies the number of intervals in the function. It must be a positive
-- integer (greater than 0). The second parameter, which is optional, is either
-- the value "start" or "end", and specifies the point at which the change of
-- values occur within the interval. If the second parameter is omitted, it is
-- given the value "end"
steps :: Int -> TimingFunction
steps i = steps' i End
    
steps' :: Int -> StepEnum -> TimingFunction
steps' i se = TimingFunction $ fromString $ "steps(" <> show i <> "," <> tostr se <> ")" -- There might be cleaner way
    where
        tostr Start = "start"
        tostr End   = "end"

-- Lets you define your own values in a cubic-bezier function
cubicBezier :: Number -> Number -> Number -> Number -> TimingFunction
cubicBezier n1 n2 n3 n4 = TimingFunction $ fromString $ "cubic-bezier(" <> numSeqStr <> ")"
    where
        numSeqStr = intercalate "," (show <$> [ n1, n2, n3, n4 ])
        
-- Sets this property to its default value
initial :: TimingFunction
initial = TimingFunction $ fromString "initial"

-- Inherits this property from its parent element
inherit :: TimingFunction
inherit = TimingFunction $ fromString "inherit"
