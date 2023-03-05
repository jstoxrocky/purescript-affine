module Test.DivisionSpec where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Data.Either (Either(..))
import Data.TransformationMatrix.Division (divide)

divisionSpec :: Spec Unit
divisionSpec = do
  describe "DivisionSpec" do
    it "should calculate the length squared of a Vector3" do
      let
        -- Expectation
        expectedResult = 2.5
        
        -- Test
        maybeResult = divide 5.0 2.0
      case maybeResult of
        Left err -> fail $ show err
        Right result -> do
          result `shouldEqual` expectedResult