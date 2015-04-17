module Css.Render where

import Css.Property
import Css.Selector
import Css.String
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

getInline :: Inline -> String
getInline (Inline s) = s

instance semigroupInline :: Semigroup Inline where
  (<>) (Inline a) (Inline b) = Inline (a <> b)

instance monoidInline :: Monoid Inline where
  mempty = Inline mempty

newtype Sheet = Sheet String

getSheet :: Sheet -> String
getSheet (Sheet s) = s

instance semigroupFile :: Semigroup Sheet where
  (<>) (Sheet a) (Sheet b) = Sheet (a <> b)

instance monoidFile :: Monoid Sheet where
  mempty = Sheet mempty

type Rendered = Maybe (These Inline Sheet)

maybeThisSide :: forall a b. These a b -> Maybe a
maybeThisSide (Both a _) = Just a
maybeThisSide (That _) = Nothing
maybeThisSide (This a) = Just a

maybeThatSide :: forall a b. These a b -> Maybe b
maybeThatSide (Both _ b) = Just b
maybeThatSide (That b) = Just b
maybeThatSide (This _) = Nothing

renderedInline :: Rendered -> Maybe String
renderedInline = (>>= ((getInline <$>) <<< maybeThisSide))

renderedSheet :: Rendered -> Maybe String
renderedSheet = (>>= ((getSheet <$>) <<< maybeThatSide))

render :: forall a. StyleM a -> Rendered
render = rules [] <<< runS

rules :: [App] -> [Rule] -> Rendered
rules sel rs = topRules <> nestedSheets
  where property (Property k v) = Just (Tuple k v)
        property _              = Nothing
        nested   (Nested a ns)  = Just (Tuple a ns)
        nested   _              = Nothing
        topRules = rule' sel (mapMaybe property rs)
        nestedSheets = intercalate (Just (That (Sheet " "))) $ uncurry nestedRules <$> mapMaybe nested rs
        nestedRules a = rules (a : sel)

rule' :: forall a. [App] -> [Tuple (Key a) Value] -> Rendered
rule' sel props = maybe q o $ nel sel
  where p = props >>= collect
        q = (This <<< Inline <<< properties <<< NEL.toArray) <$> nel p
        o sel' = Just <<< That <<< Sheet $ intercalate " " [selector (merger sel'), "{", properties p, "}"]

selector :: Selector -> String
selector = intercalate ", " <<< selector'

selector' :: Selector -> [String]
selector' (Selector (Refinement ft) p) = (<> (foldMap predicate (sort ft))) <$> selector'' ft p

selector'' :: [Predicate] -> Path Selector -> [String]
selector'' [] Star = ["*"]
selector'' (_:_) Star = [""]
selector'' _ (Elem t) = [t]
selector'' _ (PathChild a b) = sepWith " > " <$> selector' a <*> selector' b
selector'' _ (Deep a b) = sepWith " " <$> selector' a <*> selector' b
selector'' _ (Adjacent a b) = sepWith " + " <$> selector' a <*> selector' b
selector'' _ (Combined a b) = selector' a <> selector' b

sepWith :: String -> String -> String -> String
sepWith s a b = a <> s <> b

collect :: forall a. Tuple (Key a) Value -> [Either String (Tuple String String)]
collect (Tuple (Key ky) (Value v1)) = collect' ky v1

collect' :: Prefixed -> Prefixed -> [Either String (Tuple String String)]
collect' (Plain k) (Plain v) = [Right (Tuple k v)]
collect' (Prefixed ks) (Plain v) = (\(Tuple p k) -> Right $ Tuple (p <> k) v) <$> ks
collect' (Plain k) (Prefixed vs) = (\(Tuple p v) -> Right $ Tuple k (p <> v)) <$> vs
collect' (Prefixed ks) (Prefixed vs) = (\(Tuple p k) -> maybe (Left (p <> k)) (Right <<< Tuple (p <> k) <<< (p <>)) $ lookup p vs) <$> ks

properties :: [Either String (Tuple String String)] -> String
properties xs = intercalate "; " $  sheetRules <$> xs
  where sheetRules = either (\_ -> mempty) (\(Tuple k v) -> mconcat [k, ": ", v])

merger :: NEL.NonEmpty App -> Selector
merger (NEL.NonEmpty x xs) =
  case x of
    Child s -> maybe s (\xs' -> merger xs' |> s) $ nel xs
    Sub s   -> maybe s (\xs' -> merger xs' ** s) $ nel xs
    Root s  -> maybe s (\xs' -> s ** merger xs') $ nel xs
    Pop i   -> maybe (element "TODO") merger <<< nel <<< NEL.drop i $ x NEL.:| xs
    Self  sheetRules  -> maybe (star `with`  sheetRules) (\xs' -> merger xs' `with`  sheetRules) $ nel xs

predicate :: Predicate -> String
predicate (Id a) = "#" <> a
predicate (Class a) = "." <> a
predicate (Attr a) = "[" <> a <> "]"
predicate (AttrVal a v) = "[" <> a <> "='" <> v <> "']"
predicate (AttrBegins a v) = "[" <> a <> "^='" <> v <> "']"
predicate (AttrEnds a v) = "[" <> a <> "$='" <> v <> "']"

nel :: forall a. [a] -> Maybe (NEL.NonEmpty a)
nel [] = Nothing
nel (x:xs) = Just $ x NEL.:| xs
