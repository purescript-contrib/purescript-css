module CSS.Stylesheet where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Writer (Writer(), execWriter)
import Control.Monad.Writer.Class (tell)

import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(), (:|))
import Data.Profunctor.Strong (second)
import Data.Tuple (Tuple(..))

import CSS.Property (Val, Key(), Value(), cast, value)
import CSS.Selector (Selector(), Refinement())

newtype MediaType = MediaType Value

data NotOrOnly = Not | Only

data MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType (NonEmpty Array Feature)

data Feature = Feature String (Maybe Value)

data App = Self   Refinement
         | Root   Selector
         | Pop    Int
         | Child  Selector
         | Sub    Selector

data Keyframes = Keyframes String (NonEmpty Array (Tuple Number (Array Rule)))

data Rule = Property (Key Unit) Value
          | Nested   App (Array Rule)
          | Query    MediaQuery (Array Rule)
          | Face     (Array Rule)
          | Keyframe Keyframes
          | Import   String

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

infixr 5 ?
(?) :: Selector -> CSS -> CSS
(?) sel rs = rule $ Nested (Sub sel) (runS rs)

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
