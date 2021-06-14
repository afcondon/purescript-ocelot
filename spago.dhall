{-
Welcome to a Spago project!
You can edit this file as you like.

Authors:
  "Dave Zuch <https://github.com/whoadave>",
  "Chris Cornwell <https://github.com/crcornwell>",
  "Thomas Honeyman <https://github.com/thomashoneyman>",
  "Forest Toney <https://github.com/foresttoney>",
  "Hardy Jones <https://github.com/joneshf-cn>",
  "Vance Palacio <https://github.com/vanceism7>"
  "Gabe Johnson <https://github.com/citizengabe>"
-}

{ name = "ocelot"
, dependencies =
  [ "aff-promise"
  , "aff"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "bigints"
  , "control"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "formatters"
  , "fuzzy"
  , "halogen-renderless"
  , "halogen-select"
  , "halogen-subscriptions"
  , "halogen-storybook"
  , "halogen-svg-elems"
  , "halogen-vdom"
  , "halogen"
  , "html-parser-halogen"
  , "integers"
  , "js-uri"
  , "lists"
  , "maybe"
  , "media-types"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "rationals"
  , "read"
  , "remotedata"
  , "strings"
  , "svg-parser-halogen"
  , "tailrec"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  , "variant"
  , "web-dom"
  , "web-events"
  , "web-file"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
