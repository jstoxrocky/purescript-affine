module Test.Matrix4Spec where

import Prelude hiding (add)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Data.Number (sqrt)
import Data.Either (Either(..))
import Data.TransformationMatrix.Vector3 (Vector3(..))
import Data.TransformationMatrix.Matrix4 (Matrix4(..), identity4, translate)

matrix4Spec :: Spec Unit
matrix4Spec = do
  describe "Matrix4Spec" do
    it "should get the position of a Matrix4" do
      pure unit

    it "should translate a Matrix4" do
      let
        -- Setup
        matrix = Matrix4
          1.0 0.0 0.0 1.0
          0.0 1.0 0.0 2.0
          0.0 0.0 1.0 3.0
          0.0 0.0 0.0 1.0
        translation = Vector3 2.0 3.0 4.0
      
        -- Expectations
        expectedResult = Matrix4
          1.0 0.0 0.0 3.0
          0.0 1.0 0.0 5.0
          0.0 0.0 1.0 7.0
          0.0 0.0 0.0 1.0
        
        -- Test
        result = translate translation matrix
      result `shouldEqual` expectedResult
