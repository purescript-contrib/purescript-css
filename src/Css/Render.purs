module Css.Render where

import Css.Property
import Css.Selector
import Css.Stylesheet
import Data.Array
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.These
import Data.Tuple
import qualified Data.Array.NonEmpty as NEL

newtype Inline = Inline String

instance semigroupInline :: Semigroup Inline where
  (<>) (Inline a) (Inline b) = Inline (a <> b)

instance monoidInline :: Monoid Inline where
  mempty = Inline mempty

newtype Sheet = Sheet String

instance semigroupFile :: Semigroup Sheet where
  (<>) (Sheet a) (Sheet b) = Sheet (a <> b)

instance monoidFile :: Monoid Sheet where
  mempty = Sheet mempty

type Rendered = These Inline Sheet

render :: forall a. StyleM a -> Rendered
render = rules [] <<< runS

rules :: [App] -> [Rule] -> Rendered
rules sel rs = rule' sel $ mapMaybe property rs
  where property (Property k v) = Just (Tuple k v)
        property _ = Nothing

rule' :: forall a. [App] -> [Tuple (Key a) Value] -> Rendered
rule' sel props = maybe (This $ Inline p) (\sel' -> That <<< Sheet $ mconcat [selector (merger sel'), "{", p, "}"]) $ nel sel
  where p = properties $ props >>= collect

selector :: Selector -> String
selector _ = "TODO"

collect :: forall a. Tuple (Key a) Value -> [Either String (Tuple String String)]
collect (Tuple (Key ky) (Value v1)) =
  case (Tuple ky v1) of
    Tuple (Plain k) (Plain v) -> [Right (Tuple k v)]

properties :: [Either String (Tuple String String)] -> String
properties xs = intercalate "; " $ f <$> xs
  where f (Right (Tuple k v)) = mconcat [k, ": ", v]

merger :: NEL.NonEmpty App -> Selector
merger (NEL.NonEmpty x xs) =
  case x of
    Child s -> maybe s (\xs' -> merger xs' |> s) $ nel xs
    Sub s   -> maybe s (\xs' -> merger xs' ** s) $ nel xs
    Root s  -> maybe s (\xs' -> s ** merger xs') $ nel xs
    Pop i   -> maybe (element "TODO") merger <<< nel <<< NEL.drop i $ x NEL.:| xs
    Self f  -> maybe (star `with` f) (\xs' -> merger xs' `with` f) $ nel xs

nel :: forall a. [a] -> Maybe (NEL.NonEmpty a)
nel [] = Nothing
nel (x:xs) = Just $ x NEL.:| xs
