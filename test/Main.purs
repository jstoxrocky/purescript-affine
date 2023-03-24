module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import TransformationMatrix.Tests.Vector3Spec (vector3Spec)
import TransformationMatrix.Tests.DivisionSpec (divisionSpec)
import TransformationMatrix.Tests.Matrix4Spec (matrix4Spec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  vector3Spec
  divisionSpec
  matrix4Spec
