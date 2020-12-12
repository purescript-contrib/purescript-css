{ name = "css"
, dependencies =
  [ "colors"
  , "console"
  , "effect"
  , "exceptions"
  , "generics-rep"
  , "nonempty"
  , "profunctor"
  , "psci-support"
  , "strings"
  , "these"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
