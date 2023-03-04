module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Vector3Spec (vector3Spec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  vector3Spec
