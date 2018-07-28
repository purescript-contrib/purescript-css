module CSS.PseudoClass where

import CSS.String (class IsString)
import Data.Eq (class Eq)
import Data.Ord (class Ord)

data PseudoClass
  = Active
  | Any
  | AnyLink
  | Checked
  | Default
  | Defined
  | Disabled
  | Empty
  | Enabled
  | First
  | FirstChild
  | FirstOfType
  | Fullscreen
  | Focus
  | FocusVisible
  | Hover
  | Indeterminate
  | InRange
  | Invalid
  | LastChild
  | LastOfType
  | Left
  | Link
  | OnlyChild
  | OnlyOfType
  | Optional
  | OutOfRange
  | ReadOnly
  | ReadWrite
  | Required
  | Right
  | Root
  | Scope
  | Target
  | Valid
  | Visited
  | Raw String

derive instance eqPseudoClass :: Eq PseudoClass
derive instance ordPseudoClass :: Ord PseudoClass

instance isStringPseudoClass ∷ IsString PseudoClass where
  fromString = case _ of
    "active" -> Active
    "any" -> Any
    "any-link" -> AnyLink
    "checked" -> Checked
    "default" -> Default
    "defined" -> Defined
    "disabled" -> Disabled
    "empty" -> Empty
    "enabled" -> Enabled
    "first" -> First
    "first-child" -> FirstChild
    "first-of-type" -> FirstOfType
    "fullscreen" -> Fullscreen
    "focus" -> Focus
    "focus-visible" -> FocusVisible
    "hover" -> Hover
    "indeterminate" -> Indeterminate
    "in-range" -> InRange
    "invalid" -> Invalid
    "last-child" -> LastChild
    "last-of-type" -> LastOfType
    "left" -> Left
    "link" -> Link
    "only-child" -> OnlyChild
    "only-of-type" -> OnlyOfType
    "optional" -> Optional
    "out-of-range" -> OutOfRange
    "read-only" -> ReadOnly
    "read-write" -> ReadWrite
    "required" -> Required
    "right" -> Right
    "root" -> Root
    "scope" -> Scope
    "target" -> Target
    "valid" -> Valid
    "visited" -> Visited
    s -> Raw s

pseudoClassName :: PseudoClass -> String
pseudoClassName = case _ of
  Active -> "active"
  Any -> "any"
  AnyLink -> "any-link"
  Checked -> "checked"
  Default -> "default"
  Defined -> "defined"
  Disabled -> "disabled"
  Empty -> "empty"
  Enabled -> "enabled"
  First -> "first"
  FirstChild -> "first-of-child"
  FirstOfType -> "first-of-type"
  Fullscreen -> "fullscreen"
  Focus -> "focus"
  FocusVisible -> "focus-visible"
  Hover -> "hover"
  Indeterminate -> "indeterminate"
  InRange -> "in-range"
  Invalid -> "invalid"
  LastChild -> "last-child"
  LastOfType -> "last-of-type"
  Left -> "left"
  Link -> "link"
  OnlyChild -> "only-child"
  OnlyOfType -> "only-of-type"
  Optional -> "optional"
  OutOfRange -> "out-of-range"
  ReadOnly -> "read-only"
  ReadWrite -> "read-write"
  Required -> "required"
  Right -> "right"
  Root -> "root"
  Scope -> "scope"
  Target -> "target"
  Valid -> "valid"
  Visited -> "visited"
  Raw s -> s
