module CSS.Render where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Array (null, (:), drop, sort, uncons, mapMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (fold, foldMap, intercalate)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.NonEmpty (NonEmpty(..), (:|), foldl1, oneOf)
import Data.These (These(..), theseLeft, theseRight)
import Data.Tuple (Tuple(..), lookup, uncurry)

import CSS.Property (Key(..), Prefixed(..), Value(..), plain)
import CSS.Selector (Path(..), Predicate(..), Refinement(..), Selector(..), with, star, element, (**), (|>))
import CSS.String (fromString)
import CSS.Stylesheet (CSS, StyleM, App(..), Feature(..), Keyframes(..), MediaQuery(..), MediaType(..), Rule(..), runS)

newtype Inline = Inline String

derive instance eqInline :: Eq Inline
derive instance ordInline :: Ord Inline
derive instance genericInline :: Generic Inline

getInline :: Inline -> String
getInline (Inline s) = s

instance semigroupInline :: Semigroup Inline where
  append (Inline a) (Inline b) = Inline (a <> b)

instance monoidInline :: Monoid Inline where
  mempty = Inline mempty

newtype Sheet = Sheet String

derive instance eqSheet :: Eq Sheet
derive instance ordSheet :: Ord Sheet
derive instance genericSheet :: Generic Sheet

getSheet :: Sheet -> String
getSheet (Sheet s) = s

instance semigroupFile :: Semigroup Sheet where
  append (Sheet a) (Sheet b) = Sheet (a <> b)

instance monoidFile :: Monoid Sheet where
  mempty = Sheet mempty

type Rendered = Maybe (These Inline Sheet)

renderedInline :: Rendered -> Maybe String
renderedInline = (_ >>= (map getInline <<< theseLeft))

renderedSheet :: Rendered -> Maybe String
renderedSheet = (_ >>= (map getSheet <<< theseRight))

render :: forall a. StyleM a -> Rendered
render = rules [] <<< runS

putInline :: forall e. CSS -> Eff (console :: CONSOLE | e) Unit
putInline s =
  log <<< fromMaybe "" <<< renderedInline <<< render $ s

putStyleSheet :: forall e. CSS -> Eff (console :: CONSOLE | e) Unit
putStyleSheet s =
  log <<< fromMaybe "" <<< renderedSheet <<< render $ s

kframe :: Keyframes -> Rendered
kframe (Keyframes ident xs) =
  Just $ That $ Sheet $ allKeywordsWithContent
  where
  renderContent =
    " " <> ident <> " { " <> intercalate " " (uncurry frame <$> xs) <> " }\n"
  keywords =
    [ "@keyframes"
    , "@-webkit-keyframes"
    , "@-moz-keyframes"
    , "@-o-keyframes"
    ]
  allKeywordsWithContent =
    fold $ map (_ <> renderContent) keywords

frame :: Number -> Array Rule -> String
frame p rs = show p <> "% " <> "{ " <> x <> " }"
  where x = fromMaybe "" <<< renderedInline $ rules [] rs

query' :: MediaQuery -> Array App -> Array Rule -> Rendered
query' q sel rs = Just <<< That <<< Sheet $ mediaQuery q <> " { " <> fromMaybe "" (renderedSheet $ rules sel rs) <> " }\n"

mediaQuery :: MediaQuery -> String
mediaQuery (MediaQuery no ty fs) = "@media " <> mediaType ty <> foldl1 (<>) ((" and " <> _) <<< feature <$> fs)

mediaType :: MediaType -> String
mediaType (MediaType (Value s)) = plain s

feature :: Feature -> String
feature (Feature k mv) = maybe k (\(Value v) -> "(" <> k <> ": " <> plain v <> ")") mv

face :: Array Rule -> Rendered
face rs = Just <<< That <<< Sheet $ "@font-face { " <> fromMaybe "" (renderedInline $ rules [] rs) <> " }\n"

rules :: Array App -> Array Rule -> Rendered
rules sel rs = topRules <> importRules <> keyframeRules <> faceRules <> nestedSheets <> queryRules
  where property (Property k v) = Just (Tuple k v)
        property _              = Nothing
        nested   (Nested a ns ) = Just (Tuple a ns)
        nested   _              = Nothing
        queries  (Query  q ns ) = Just (Tuple q ns)
        queries  _              = Nothing
        kframes  (Keyframe fs ) = Just fs
        kframes  _              = Nothing
        faces    (Face ns     ) = Just ns
        faces    _              = Nothing
        imports  (Import i    ) = Just i
        imports  _              = Nothing
        topRules      = if not null rs'
                          then rule' sel rs'
                          else Nothing
          where rs' = mapMaybe property rs
        nestedSheets  = fold $ uncurry nestedRules <$> mapMaybe nested rs
        nestedRules a = rules (a : sel)
        queryRules    = foldMap (uncurry $ flip query' sel) $ mapMaybe queries rs
        keyframeRules = foldMap kframe $ mapMaybe kframes rs
        faceRules     = foldMap face   $ mapMaybe faces   rs
        importRules   = foldMap imp    $ mapMaybe imports rs

imp :: String -> Rendered
imp t = Just <<< That <<< Sheet <<< fromString $ "@import url(" <> t <> ");\n"

rule' :: forall a. Array App -> Array (Tuple (Key a) Value) -> Rendered
rule' sel props = maybe q o $ nel sel
  where p = props >>= collect
        q = (This <<< Inline <<< properties <<< oneOf) <$> nel p
        o sel' = Just <<< That <<< Sheet $ intercalate " " [selector (merger sel'), "{", properties p, "}\n"]

selector :: Selector -> String
selector = intercalate ", " <<< selector'

selector' :: Selector -> Array String
selector' (Selector (Refinement ft) p) = (_ <> (foldMap predicate (sort ft))) <$> selector'' ft p

selector'' :: Array Predicate -> Path Selector -> Array String
selector'' [] Star = ["*"]
selector'' _  Star = [""]
selector'' _ (Elem t) = [t]
selector'' _ (PathChild a b) = sepWith " > " <$> selector' a <*> selector' b
selector'' _ (Deep a b) = sepWith " " <$> selector' a <*> selector' b
selector'' _ (Adjacent a b) = sepWith " + " <$> selector' a <*> selector' b
selector'' _ (Combined a b) = selector' a <> selector' b

sepWith :: String -> String -> String -> String
sepWith s a b = a <> s <> b

collect :: forall a. Tuple (Key a) Value -> Array (Either String (Tuple String String))
collect (Tuple (Key ky) (Value v1)) = collect' ky v1

collect' :: Prefixed -> Prefixed -> Array (Either String (Tuple String String))
collect' (Plain k) (Plain v) = [Right (Tuple k v)]
collect' (Prefixed ks) (Plain v) = (\(Tuple p k) -> Right $ Tuple (p <> k) v) <$> ks
collect' (Plain k) (Prefixed vs) = (\(Tuple p v) -> Right $ Tuple k (p <> v)) <$> vs
collect' (Prefixed ks) (Prefixed vs) = (\(Tuple p k) -> maybe (Left (p <> k)) (Right <<< Tuple (p <> k) <<< (p <> _)) $ lookup p vs) <$> ks

properties :: Array (Either String (Tuple String String)) -> String
properties xs = intercalate "; " $  sheetRules <$> xs
  where sheetRules = either (\_ -> mempty) (\(Tuple k v) -> fold [k, ": ", v])

merger :: NonEmpty Array App -> Selector
merger (NonEmpty x xs) =
  case x of
    Child s -> maybe s (\xs' -> merger xs' |> s) $ nel xs
    Sub s   -> maybe s (\xs' -> merger xs' ** s) $ nel xs
    Root s  -> maybe s (\xs' -> s ** merger xs') $ nel xs
    Pop i   -> maybe (element "TODO") merger <<< nel <<< drop i $ x : xs
    Self  sheetRules  -> maybe (star `with`  sheetRules) (\xs' -> merger xs' `with`  sheetRules) $ nel xs

predicate :: Predicate -> String
predicate (Id           a  ) = "#" <> a
predicate (Class        a  ) = "." <> a
predicate (Attr         a  ) = "[" <> a <> "]"
predicate (AttrVal      a v) = "[" <> a <> "='" <> v <> "']"
predicate (AttrBegins   a v) = "[" <> a <> "^='" <> v <> "']"
predicate (AttrEnds     a v) = "[" <> a <> "$='" <> v <> "']"
predicate (AttrContains a v) = "[" <> a <> "*='" <> v <> "']"
predicate (AttrSpace    a v) = "[" <> a <> "~='" <> v <> "']"
predicate (AttrHyph     a v) = "[" <> a <> "|='" <> v <> "']"
predicate (Pseudo       a  ) = ":" <> a
predicate (PseudoFunc   a p) = ":" <> a <> "(" <> intercalate "," p <> ")"

nel :: forall a. Array a -> Maybe (NonEmpty Array a)
nel [] = Nothing
nel xs = (\{ head: head, tail: tail } -> head :| tail) <$> uncons xs
