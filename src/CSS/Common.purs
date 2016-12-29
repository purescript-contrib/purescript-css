-- | A bunch of type classes representing common values shared between multiple
-- CSS properties, like `Auto`, `Inherit`, `None`, `Normal` and several more.
--
-- All the common value type classes have an instance for the Value type,
-- making them easily derivable for custom value types.

module CSS.Common where

import Prelude

import Data.Monoid (class Monoid)
import Data.Tuple (Tuple(..))

import CSS.Property (Prefixed(..), Value)
import CSS.String (class IsString, fromString)

-------------------------------------------------------------------------------

class All      a where all      :: a
class Auto     a where auto     :: a
class Baseline a where baseline :: a
class Center   a where center   :: a
class Inherit  a where inherit  :: a
class None     a where none     :: a
class Normal   a where normal   :: a
class Visible  a where visible  :: a
class Hidden   a where hidden   :: a
class Initial  a where initial  :: a
class Unset    a where unset    :: a
class Top      a where top      :: a
class Middle   a where middle   :: a
class Bottom   a where bottom   :: a
class URL      a where url      :: String -> a

-- | The other type class is used to escape from the type safety introduced by
-- embedding CSS properties into the typed world of purescript-css.
-- `Other` allows you to cast any `Value` to a specific value type.

class Other    a where other    :: Value -> a

instance allValue      :: All      Value where all      = fromString "all"
instance autoValue     :: Auto     Value where auto     = fromString "auto"
instance baselineValue :: Baseline Value where baseline = fromString "baseline"
instance centerValue   :: Center   Value where center   = fromString "center"
instance inheritValue  :: Inherit  Value where inherit  = fromString "inherit"
instance normalValue   :: Normal   Value where normal   = fromString "normal"
instance noneValue     :: None     Value where none     = fromString "none"
instance visibleValue  :: Visible  Value where visible  = fromString "visible"
instance hiddenValue   :: Hidden   Value where hidden   = fromString "hidden"
instance otherValue    :: Other    Value where other    = id
instance initialValue  :: Initial  Value where initial  = fromString "initial"
instance unsetValue    :: Unset    Value where unset    = fromString "unset"
instance topValue      :: Top      Value where top      = fromString "top"
instance middleValue   :: Middle   Value where middle   = fromString "middle"
instance bottomValue   :: Bottom   Value where bottom   = fromString "bottom"
instance urlValue :: URL Value where url s = fromString ("url(\"" <> s <> "\")")

-------------------------------------------------------------------------------

-- | Common list browser prefixes to make experimental properties work in
-- different browsers.

browsers :: Prefixed
browsers = Prefixed
  [ Tuple "-webkit-" ""
  , Tuple    "-moz-" ""
  , Tuple     "-ms-" ""
  , Tuple      "-o-" ""
  , Tuple         "" ""
  ]

-------------------------------------------------------------------------------

-- | Syntax for CSS function call.

call :: forall s. (IsString s, Monoid s) => s -> s -> s
call fn arg = fn <> fromString "(" <> arg <> fromString ")"
