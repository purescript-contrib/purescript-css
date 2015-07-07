module Css.Stylesheet where

import Prelude
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Css.Property
import Css.Selector
import Data.Array (singleton)
import Data.Maybe
import Data.Profunctor.Strong
import Data.Tuple
import qualified Data.Array.NonEmpty as NEL

newtype MediaType = MediaType Value

data NotOrOnly = Not | Only

data MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType (NEL.NonEmpty Feature)

data Feature = Feature String (Maybe Value)

data App = Self   Refinement
         | Root   Selector
         | Pop    Int
         | Child  Selector
         | Sub    Selector

data Keyframes = Keyframes String (NEL.NonEmpty (Tuple Number (Array Rule)))

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

rule :: Rule -> Css
rule = S <<< tell <<< singleton

type Css = StyleM Unit

key :: forall a. (Val a) => Key a -> a -> Css
key k v = rule $ Property (cast k) (value v)

infixr 5 ?
(?) :: Selector -> Css -> Css
(?) sel rs = rule $ Nested (Sub sel) (runS rs)

query :: MediaType -> NEL.NonEmpty Feature -> Css -> Css
query ty fs = rule <<< Query (MediaQuery Nothing ty fs) <<< runS

keyframes :: String -> NEL.NonEmpty (Tuple Number Css) -> Css
keyframes n xs = rule $ Keyframe (Keyframes n (second runS <$> xs))

keyframesFromTo :: String -> Css -> Css -> Css
keyframesFromTo n a b = keyframes n $ Tuple 0.0 a NEL.:| [Tuple 100.0 b]

fontFace :: Css -> Css
fontFace = rule <<< Face <<< runS

importUrl :: String -> Css
importUrl = rule <<< Import
