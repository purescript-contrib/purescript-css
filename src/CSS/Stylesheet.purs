module CSS.Stylesheet where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array (singleton)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor.Strong (second)
import Data.Tuple (Tuple(..))

import CSS.Property (class Val, Key(..), Prefixed, Value, cast, value)
import CSS.Selector (Selector, Refinement)

newtype MediaType = MediaType Value

derive instance eqMediaType :: Eq MediaType
derive instance ordMediaType:: Ord MediaType
derive instance genericMediaType :: Generic MediaType

data NotOrOnly = Not | Only

derive instance eqNotOrOnly :: Eq NotOrOnly
derive instance ordNotOrOnly:: Ord NotOrOnly
derive instance genericNotOrOnly :: Generic NotOrOnly

data MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType (NonEmpty Array Feature)

derive instance eqMediaQuery :: Eq MediaQuery
derive instance ordMediaQuery :: Ord MediaQuery
derive instance genericMediaQuery :: Generic MediaQuery

data Feature = Feature String (Maybe Value)

derive instance eqFeature :: Eq Feature
derive instance ordFeature :: Ord Feature
derive instance genericFeature :: Generic Feature

data App
  = Self Refinement
  | Root Selector
  | Pop Int
  | Child Selector
  | Sub Selector

derive instance eqApp :: Eq App
derive instance ordApp :: Ord App
derive instance genericApp :: Generic App

data Keyframes = Keyframes String (NonEmpty Array (Tuple Number (Array Rule)))

derive instance eqKeyframes :: Eq Keyframes
derive instance ordKeyframes :: Ord Keyframes
derive instance genericKeyframes :: Generic Keyframes

data Rule
  = Property (Key Unit) Value
  | Nested App (Array Rule)
  | Query MediaQuery (Array Rule)
  | Face (Array Rule)
  | Keyframe Keyframes
  | Import String

derive instance eqRule :: Eq Rule
derive instance ordRule :: Ord Rule
derive instance genericRule :: Generic Rule

newtype StyleM a = S (Writer (Array Rule) a)

instance functorStyleM :: Functor StyleM where
  map f (S w) = S $ f <$> w

instance applyStyleM :: Apply StyleM where
  apply (S f) (S w) = S $ f <*> w

instance bindStyleM :: Bind StyleM where
  bind (S w) f = S $ w >>= (\(S w') -> w') <<< f

instance applicativeStyleM :: Applicative StyleM where
  pure = S <<< pure

instance monadStyleM :: Monad StyleM

runS :: forall a. StyleM a -> Array Rule
runS (S s) = execWriter s

rule :: Rule -> CSS
rule = S <<< tell <<< singleton

type CSS = StyleM Unit

instance semigroupCSS :: Semigroup (StyleM Unit) where
  append = (*>)

key :: forall a. (Val a) => Key a -> a -> CSS
key k v = rule $ Property (cast k) (value v)

prefixed :: forall a. Val a => Prefixed -> a -> CSS
prefixed xs = key (Key xs)

infixr 5 select as ?
select :: Selector -> CSS -> CSS
select sel rs = rule $ Nested (Sub sel) (runS rs)

query :: MediaType -> NonEmpty Array Feature -> CSS -> CSS
query ty fs = rule <<< Query (MediaQuery Nothing ty fs) <<< runS

keyframes :: String -> NonEmpty Array (Tuple Number CSS) -> CSS
keyframes n xs = rule $ Keyframe (Keyframes n (second runS <$> xs))

keyframesFromTo :: String -> CSS -> CSS -> CSS
keyframesFromTo n a b = keyframes n $ Tuple 0.0 a :| [Tuple 100.0 b]

fontFace :: CSS -> CSS
fontFace = rule <<< Face <<< runS

importUrl :: String -> CSS
importUrl = rule <<< Import
