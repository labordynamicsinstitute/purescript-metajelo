{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}


let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200226/packages.dhall sha256:3a52562e05b31a7b51d12d5b228ccbe567c527781a88e9028ab42374ab55c0f1

let overrides = { metajelo = ./spago.dhall as Location }

let additions = {
        enums =
          { dependencies =
            [ "control"
            , "either"
            , "gen"
            , "maybe"
            , "newtype"
            , "nonempty"
            , "partial"
            , "prelude"
            , "tuples"
            , "unfoldable"
            ]
          , repo = "https://github.com/bbarker/purescript-enums.git"
          , version = "1979eb74baec39b5e62567948f402b4194230e9f"
          }
        , url-validator =
          { dependencies =
            [ "nullable" ]
          , repo = "https://github.com/bbarker/purescript-url-validator.git"
          , version = "v2.1.0"
          }
        , web-dom-parser =
          { dependencies =
            [ "prelude", "effect", "partial", "web-dom" ]
          , repo = "https://github.com/purescript-web/purescript-web-dom-parser.git"
          , version =  "v6.1.1"
          }
        , web-dom-xpath =
          { dependencies =
            [ "prelude", "effect", "partial", "web-dom" ]
          , repo = "https://github.com/purescript-web/purescript-web-dom-xpath.git"
          , version = "v1.2.1"
          }
        , xpath-like =
          { dependencies =
            [ "prelude" ]
          , repo = "https://github.com/bbarker/purescript-xpath-like.git"
          , version =  "v3.0.0"
          }
      }

in  upstream // overrides // additions
