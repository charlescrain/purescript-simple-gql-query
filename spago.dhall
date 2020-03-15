{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "simple-gql-query"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "console"
  , "effect"
  , "exceptions"
  , "generics-rep"
  , "node-fs-aff"
  , "parsing"
  , "prelude"
  , "prettier"
  , "psci-support"
  , "simple-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
