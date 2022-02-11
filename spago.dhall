{ name = "css"
, dependencies =
  [ "arrays"
  , "colors"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "exists"
  , "foldable-traversable"
  , "maybe"
  , "nonempty"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "strings"
  , "these"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
