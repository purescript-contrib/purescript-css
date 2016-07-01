-- | CSS Flexible Box Layout
-- | http://dev.w3.org/csswg/css-flexbox-1
module CSS.Flexbox where

import Prelude

import Data.Generic (class Generic)

import CSS.Common (class Center, class Inherit, class Other, class Baseline, class Auto)
import CSS.Property (class Val, Value, value, (!))
import CSS.Size (Size)
import CSS.String (class IsString, fromString)
import CSS.Stylesheet (CSS, key)

class FlexEnd a where
  flexEnd :: a

class FlexStart a where
  flexStart :: a

class SpaceAround a where
  spaceAround :: a

class SpaceBetween a where
  spaceBetween :: a

class Stretch a where
  stretch :: a

instance flexEndValue :: FlexEnd Value where
  flexEnd = fromString "flex-end"

instance flexStartValue :: FlexStart Value where
  flexStart = fromString "flex-start"

instance spaceAroundValue :: SpaceAround Value where
  spaceAround = fromString "space-around"

instance spaceBetweenValue :: SpaceBetween Value where
  spaceBetween = fromString "space-between"

instance stretchValue :: Stretch Value where
  stretch = fromString "stretch"

-------------------------------------------------------------------------------

newtype AlignContentValue = AlignContentValue Value

derive instance eqAlignContentValue :: Eq AlignContentValue
derive instance ordAlignContentValue :: Ord AlignContentValue
derive instance genericAlignContentValue :: Generic AlignContentValue

instance isStringAlignContentValue :: IsString AlignContentValue where
  fromString = AlignContentValue <<< fromString

instance valAlignContentValue :: Val AlignContentValue where
  value (AlignContentValue v) = v

instance otherAlignContentValue :: Other AlignContentValue where
  other = AlignContentValue

instance inheritAlignContentValue :: Inherit AlignContentValue where
  inherit = fromString "inherit"

instance flexStartAlignContentValue :: FlexStart AlignContentValue where
  flexStart = fromString "flex-start"

instance flexEndAlignContentValue :: FlexEnd AlignContentValue where
  flexEnd = fromString "flex-end"

instance centerAlignContentValue :: Center AlignContentValue where
  center = fromString "center"

instance spaceBetweenAlignContentValue :: SpaceBetween AlignContentValue where
  spaceBetween = fromString "space-between"

instance spaceAroundAlignContentValue :: SpaceAround AlignContentValue where
  spaceAround = fromString "space-around"

instance stretchAlignContentValue :: Stretch AlignContentValue where
  stretch = fromString "stretch"

alignContent :: AlignContentValue -> CSS
alignContent = key $ fromString "align-content"

-------------------------------------------------------------------------------

newtype AlignItemsValue = AlignItemsValue Value

derive instance eqAlignItemsValue :: Eq AlignItemsValue
derive instance ordAlignItemsValue :: Ord AlignItemsValue
derive instance genericAlignItemsValue :: Generic AlignItemsValue

instance isStringAlignItemsValue :: IsString AlignItemsValue where
  fromString = AlignItemsValue <<< fromString

instance valAlignItemsValue :: Val AlignItemsValue where
  value (AlignItemsValue v) = v

instance otherAlignItemsValue :: Other AlignItemsValue where
  other = AlignItemsValue

instance inheritAlignItemsValue :: Inherit AlignItemsValue where
  inherit = fromString "inherit"

instance baselineAlignItemsValue :: Baseline AlignItemsValue where
  baseline = fromString "baseline"

instance centerAlignItemsValue :: Center AlignItemsValue where
  center = fromString "center"

instance flexEndAlignItemsValue :: FlexEnd AlignItemsValue where
  flexEnd = fromString "flex-end"

instance flexStartAlignItemsValue :: FlexStart AlignItemsValue where
  flexStart = fromString "flex-start"

instance stretchAlignItemsValue :: Stretch AlignItemsValue where
  stretch = fromString "stretch"

alignItems :: AlignItemsValue -> CSS
alignItems = key $ fromString "align-items"

-------------------------------------------------------------------------------

newtype AlignSelfValue = AlignSelfValue Value

derive instance eqAlignSelfValue :: Eq AlignSelfValue
derive instance ordAlignSelfValue :: Ord AlignSelfValue
derive instance genericAlignSelfValue :: Generic AlignSelfValue

instance isStringAlignSelfValue :: IsString AlignSelfValue where
  fromString = AlignSelfValue <<< fromString

instance valAlignSelfValue :: Val AlignSelfValue where
  value (AlignSelfValue v) = v

instance otherAlignSelfValue :: Other AlignSelfValue where
  other = AlignSelfValue

instance inheritAlignSelfValue :: Inherit AlignSelfValue where
  inherit = fromString "inherit"

instance autoAlignSelfValue :: Auto AlignSelfValue where
  auto = fromString "auto"

instance baselineAlignSelfValue :: Baseline AlignSelfValue where
  baseline = fromString "baseline"

instance centerAlignSelfValue :: Center AlignSelfValue where
  center = fromString "center"

instance flexEndAlignSelfValue :: FlexEnd AlignSelfValue where
  flexEnd = fromString "flex-end"

instance flexStartAlignSelfValue :: FlexStart AlignSelfValue where
  flexStart = fromString "flex-start"

instance stretchAlignSelfValue :: Stretch AlignSelfValue where
  stretch = fromString "stretch"

alignSelf :: AlignSelfValue -> CSS
alignSelf = key $ fromString "align-self"

-------------------------------------------------------------------------------

flex :: forall b. Int -> Int -> Size b -> CSS
flex g s b = key (fromString "flex") (gs ! ss ! value b)
  where gs = fromString (show g) :: Value
        ss = fromString (show s) :: Value

-------------------------------------------------------------------------------

flexBasis :: forall a. Size a -> CSS
flexBasis = key $ fromString "flex-basis"

-------------------------------------------------------------------------------

newtype FlexDirection = FlexDirection Value

derive instance eqFlexDirection :: Eq FlexDirection
derive instance ordFlexDirection :: Ord FlexDirection
derive instance genericFlexDirection :: Generic FlexDirection

instance valFlexDirection :: Val FlexDirection where
  value (FlexDirection v) = v

instance otherFlexDirection :: Other FlexDirection where
  other = FlexDirection

row :: FlexDirection
row = FlexDirection $ fromString "row"

rowReverse :: FlexDirection
rowReverse = FlexDirection $ fromString "row-reverse"

column :: FlexDirection
column = FlexDirection $ fromString "column"

columnReverse :: FlexDirection
columnReverse = FlexDirection $ fromString "column-reverse"

flexDirection :: FlexDirection -> CSS
flexDirection = key $ fromString "flex-direction"

-------------------------------------------------------------------------------

flexFlow :: FlexDirection -> FlexWrap -> CSS
flexFlow d w = key (fromString "flex-flow") (d ! w)

-------------------------------------------------------------------------------

flexGrow :: Int -> CSS
flexGrow i = key (fromString "flex-grow") (fromString (show i) :: Value)

flexShrink :: Int  -> CSS
flexShrink i = key (fromString "flex-shrink") (fromString (show i) :: Value)

-------------------------------------------------------------------------------

newtype FlexWrap = FlexWrap Value

derive instance eqFlexWrap :: Eq FlexWrap
derive instance ordFlexWrap :: Ord FlexWrap
derive instance genericFlexWrap :: Generic FlexWrap

instance valFlexWrap :: Val FlexWrap where
  value (FlexWrap v) = v

instance otherFlexWrap :: Other FlexWrap where
  other = FlexWrap

nowrap :: FlexWrap
nowrap = FlexWrap $ fromString "nowrap"

wrap :: FlexWrap
wrap = FlexWrap $ fromString "wrap"

wrapReverse :: FlexWrap
wrapReverse = FlexWrap $ fromString "wrap-reverse"

flexWrap :: FlexWrap -> CSS
flexWrap = key $ fromString "flex-wrap"

-------------------------------------------------------------------------------

newtype JustifyContentValue = JustifyContentValue Value

derive instance eqJustifyContentValue :: Eq JustifyContentValue
derive instance ordJustifyContentValue :: Ord JustifyContentValue
derive instance genericJustifyContentValue :: Generic JustifyContentValue

instance isStringJustifyContentValue :: IsString JustifyContentValue where
  fromString = JustifyContentValue <<< fromString

instance valJustifyContentValue :: Val JustifyContentValue where
  value (JustifyContentValue v) = v

instance otherJustifyContentValue :: Other JustifyContentValue where
  other = JustifyContentValue

instance inheritJustifyContentValue :: Inherit JustifyContentValue where
  inherit = fromString "inherit"

instance centerJustifyContentValue :: Center JustifyContentValue where
  center = fromString "center"

instance flexEndJustifyContentValue :: FlexEnd JustifyContentValue where
  flexEnd = fromString "flex-end"

instance flexStartJustifyContentValue :: FlexStart JustifyContentValue where
  flexStart = fromString "flex-start"

instance spaceAroundJustifyContentValue :: SpaceAround JustifyContentValue
  where spaceAround = fromString "space-around"

instance spaceBetweenJustifyContentValue :: SpaceBetween JustifyContentValue
  where spaceBetween = fromString "space-between"

justifyContent :: JustifyContentValue -> CSS
justifyContent = key $ fromString "justify-content"

-------------------------------------------------------------------------------

order :: Int -> CSS
order i = key (fromString "order") (fromString (show i) :: Value)
