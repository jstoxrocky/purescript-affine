{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "affine"
, license = "BSD-3-Clause"
, repository = "https://github.com/jstoxrocky/purescript-affine"
, dependencies = 
    [ "console"
    , "effect"
    , "either"
    , "arrays"
    , "numbers"
    , "prelude"
    , "foldable-traversable"
    , "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
