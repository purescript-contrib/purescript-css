{ name = "css"
, dependencies =
  [ "arrays"
  , "colors"
  , "console"
  , "effect"
  , "either"
  , "exists"
  , "foldable-traversable"
  , "maybe"
  , "nonempty"
  , "prelude"
  , "profunctor"
  , "strings"
  , "these"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
