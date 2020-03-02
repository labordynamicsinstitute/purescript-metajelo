{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-metajelo-tests"
, dependencies = [
    "metajelo"
  , "debug"
  , "console"
  , "foreign-object"
  , "psci-support"
  , "test-unit"
  , "web-dom-parser"
]
, packages = ../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
