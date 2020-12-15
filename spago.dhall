{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "nix-purescript-example-project"
, dependencies =
    [
      "email-validate"
    , "enums"
    , "foreign-object"
    , "generics-rep"
    , "globals"
    , "js-date"
    , "naturals"
    , "nonbili-dom"
    , "stringutils"
    , "url-validator"
    , "web-dom"
    , "web-dom-parser"
    , "web-dom-xpath"
    , "xpath-like"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
