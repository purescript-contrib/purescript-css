module Css.Stylesheet where

import Control.Monad.Writer
import Control.Monad.Writer.Class
import Css.Property
import Css.Selector
import Data.Maybe
import Data.Profunctor.Strong
import Data.Tuple
import qualified Data.Array.NonEmpty as NEL

newtype MediaType = MediaType Value

data NotOrOnly = Not | Only

data MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType [Feature]

data Feature = Feature String (Maybe Value)

data App = Self   Refinement
         | Root   Selector
         | Pop    Number
         | Child  Selector
         | Sub    Selector

data Keyframes = Keyframes String (NEL.NonEmpty (Tuple Number [Rule]))

data Rule = Property (Key Unit) Value
          | Nested   App [Rule]
          | Query    MediaQuery [Rule]
          | Face     [Rule]
          | Keyframe Keyframes
          | Import   String

newtype StyleM a = S (Writer [Rule] a)

instance functorStyleM :: Functor StyleM where
  (<$>) f (S w) = S $ f <$> w

instance applyStyleM :: Apply StyleM where
  (<*>) (S f) (S w) = S $ f <*> w

instance bindStyleM :: Bind StyleM where
  (>>=) (S w) f = S $ w >>= (\(S w') -> w') <<< f

instance applicativeStyleM :: Applicative StyleM where
  pure = S <<< pure

instance monadStyleM :: Monad StyleM

runS :: forall a. StyleM a -> [Rule]
runS (S s) = execWriter s

rule :: Rule -> Css
rule = S <<< tell <<< (:[])

type Css = StyleM Unit

key :: forall a. (Val a) => Key a -> a -> Css
key k v = rule $ Property (cast k) (value v)

infixr 5 ?
(?) :: Selector -> Css -> Css
(?) sel rs = rule $ Nested (Sub sel) (runS rs)

keyframes :: String -> NEL.NonEmpty (Tuple Number Css) -> Css
keyframes n xs = rule $ Keyframe (Keyframes n (second runS <$> xs))

fontFace :: Css -> Css
fontFace = rule <<< Face <<< runS
