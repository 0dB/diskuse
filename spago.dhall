{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "diskuse"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "node-fs"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
