module CSS.Flexbox where

import Prelude (($), (<<<))

import Data.Tuple.Nested (tuple2)

import CSS.String (fromString)
import CSS.Property (Value(), Val)
import CSS.Stylesheet (CSS(), key)
import CSS.Size (Size())



newtype FlexDirection = FlexDirection Value

instance valFlexDirection :: Val FlexDirection where
    value (FlexDirection v) = v

flexDirection :: FlexDirection -> CSS
flexDirection = key $ fromString "flex-direction"

row :: FlexDirection
row = FlexDirection $ fromString "row"

rowReverse :: FlexDirection
rowReverse = FlexDirection $ fromString "row-reverse"

column :: FlexDirection
column = FlexDirection $ fromString "column"

columnReverse :: FlexDirection
columnReverse = FlexDirection $ fromString "column-reverse"



newtype FlexWrap = FlexWrap Value

instance valFlexWrap :: Val FlexWrap where
    value (FlexWrap v) = v

flexWrap :: FlexWrap -> CSS
flexWrap = key $ fromString "flex-wrap"

noWrap :: FlexWrap
noWrap = FlexWrap $ fromString "nowrap"

wrap :: FlexWrap
wrap = FlexWrap $ fromString "wrap"

wrapReverse :: FlexWrap
wrapReverse = FlexWrap $ fromString "wrap-reverse"

flexFlow :: FlexDirection -> FlexWrap -> CSS
flexFlow d w = key (fromString "flex-flow") (tuple2 d w)



newtype FlexStartEndCenter = FlexStartEndCenter Value

instance valFlexStartEndCenter :: Val FlexStartEndCenter where
    value (FlexStartEndCenter v) = v

flexStart :: FlexStartEndCenter
flexStart = FlexStartEndCenter $ fromString "flex-start"

flexEnd :: FlexStartEndCenter
flexEnd = FlexStartEndCenter $ fromString "flex-end"

center :: FlexStartEndCenter
center = FlexStartEndCenter $ fromString "center"



newtype FlexSpace = FlexSpace Value

instance valFlexSpace :: Val FlexSpace where
    value (FlexSpace v) = v

spaceBetween :: FlexSpace
spaceBetween = FlexSpace $ fromString "space-between"

spaceAround :: FlexSpace
spaceAround = FlexSpace $ fromString "space-around"



newtype FlexStretch = FlexStretch Value

instance valFlexStretch :: Val FlexStretch where
    value (FlexStretch v) = v

stretch :: FlexStretch
stretch = FlexStretch $ fromString "stretch"



newtype FlexBaseline = FlexBaseline Value

instance valFlexBaseline :: Val FlexBaseline where
    value (FlexBaseline v) = v

baseline :: FlexBaseline
baseline = FlexBaseline $ fromString "baseline"



data JustifyContent = JustifyStartEndCenter FlexStartEndCenter |
                      JustifySpace FlexSpace

instance valJustifyContent :: Val JustifyContent where
    value (JustifyStartEndCenter (FlexStartEndCenter v)) = v
    value (JustifySpace (FlexSpace v)) = v

class ToJustifyContent a where
    toJustifyContent :: a -> JustifyContent

instance toJustifyContentFlexSEC :: ToJustifyContent FlexStartEndCenter where
    toJustifyContent = JustifyStartEndCenter

instance toJustifyContentFlexSpace :: ToJustifyContent FlexSpace where
    toJustifyContent = JustifySpace

justifyContent :: forall a. (ToJustifyContent a) => a -> CSS
justifyContent = (key $ fromString "justify-content") <<< toJustifyContent



data AlignItems = AlignItemsStartEndCenter FlexStartEndCenter |
                  AlignItemsStretch FlexStretch |
                  AlignItemsBaseline FlexBaseline

instance valAlignItems :: Val AlignItems where
    value (AlignItemsStartEndCenter (FlexStartEndCenter v)) = v
    value (AlignItemsStretch (FlexStretch v)) = v
    value (AlignItemsBaseline (FlexBaseline v)) = v

class ToAlignItems a where
    toAlignItems :: a -> AlignItems

instance toAlignItemsFlexSEC :: ToAlignItems FlexStartEndCenter where
    toAlignItems = AlignItemsStartEndCenter

instance toAlignItemsFlexStretch :: ToAlignItems FlexStretch where
    toAlignItems = AlignItemsStretch

instance toAlignItemsFlexBaseline :: ToAlignItems FlexBaseline where
    toAlignItems = AlignItemsBaseline

alignItems :: forall a. (ToAlignItems a) => a -> CSS
alignItems = (key $ fromString "align-items") <<< toAlignItems



data AlignContent = AlignContentStartEndCenter FlexStartEndCenter |
                    AlignContentSpace FlexSpace |
                    AlignContentStretch FlexStretch

instance valAlignContent :: Val AlignContent where
    value (AlignContentStartEndCenter (FlexStartEndCenter v)) = v
    value (AlignContentSpace (FlexSpace v)) = v
    value (AlignContentStretch (FlexStretch v)) = v

class ToAlignContent a where
    toAlignContent :: a -> AlignContent

instance toAlignContentFlexSEC :: ToAlignContent FlexStartEndCenter where
    toAlignContent = AlignContentStartEndCenter

instance toAlignContentFlexSpace :: ToAlignContent FlexSpace where
    toAlignContent = AlignContentSpace

instance toAlignContentFlexStretch :: ToAlignContent FlexStretch where
    toAlignContent = AlignContentStretch

alignContent :: forall a. (ToAlignContent a) => a -> CSS
alignContent = (key $ fromString "align-content") <<< toAlignContent



data AlignSelf = AlignSelfStartEndCenter FlexStartEndCenter |
                 AlignSelfBaseline FlexBaseline |
                 AlignSelfStretch FlexStretch

instance valAlignSelf :: Val AlignSelf where
    value (AlignSelfStartEndCenter (FlexStartEndCenter v)) = v
    value (AlignSelfBaseline (FlexBaseline v)) = v
    value (AlignSelfStretch (FlexStretch v)) = v

class ToAlignSelf a where
    toAlignSelf :: a -> AlignSelf

instance toAlignSelfFlexSEC :: ToAlignSelf FlexStartEndCenter where
    toAlignSelf = AlignSelfStartEndCenter

instance toAlignSelfFlexBaseline :: ToAlignSelf FlexBaseline where
    toAlignSelf = AlignSelfBaseline

instance toAlignSelfFlexStretch :: ToAlignSelf FlexStretch where
    toAlignSelf = AlignSelfStretch

alignSelf :: forall a. (ToAlignSelf a) => a -> CSS
alignSelf = (key $ fromString "align-self") <<< toAlignSelf



order :: Int -> CSS
order = key (fromString "order")

flexGrow :: Int -> CSS
flexGrow = key (fromString "flex-grow")

flexShrink :: Int -> CSS
flexShrink = key (fromString "flex-shrink")

flexBasis :: forall a. Size a -> CSS
flexBasis = key (fromString "flex-basis")
