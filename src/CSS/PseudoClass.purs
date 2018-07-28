module CSS.PseudoClass where

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

derive instance eqPseudoClass :: Eq PseudoClass
derive instance ordPseudoClass :: Ord PseudoClass

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
