{ name = "github-actions-toolkit"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foreign-object"
  , "maybe"
  , "node-buffer"
  , "node-path"
  , "node-streams"
  , "nullable"
  , "prelude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/purescript-contrib/purescript-github-actions-toolkit"
}
