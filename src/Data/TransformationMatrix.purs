module Data.Affine.TransformationMatrix where

import Prelude
import Data.Affine.Vector3 as V3
import Data.Number (sin, cos)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Affine.Axis (Axis(..), X(..), Y(..), Z(..))
import Data.Affine.RotationMatrix (RotationMatrix(..), xAxisRotation, yAxisRotation, zAxisRotation)
import Data.Affine.Rotation (Radians(..), degreeToRadians)
import Data.Either (Either)
import Data.Affine.DivisionError (DivisionError)
import Data.Affine.Division (divide)

data TransformationMatrix = TransformationMatrix
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number

derive instance genericTransformationMatrix :: Generic TransformationMatrix _
instance showTransformationMatrix :: Show TransformationMatrix where
  show = genericShow

derive instance eqTransformationMatrix :: Eq TransformationMatrix

toArray :: TransformationMatrix -> Array Number
toArray (TransformationMatrix x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) = [ x11, x12, x13, x14, x21, x22, x23, x24, x31, x32, x33, x34, x41, x42, x43, x44 ]

getPosition :: TransformationMatrix -> V3.Vector3 Number
getPosition (TransformationMatrix _ _ _ x14 _ _ _ x24 _ _ _ x34 _ _ _ _) = V3.Vector3 x14 x24 x34

setPosition :: TransformationMatrix -> V3.Vector3 Number -> TransformationMatrix
setPosition
  (TransformationMatrix x11 x12 x13 _ x21 x22 x23 _ x31 x32 x33 _ x41 x42 x43 x44)
  (V3.Vector3 y11 y21 y31) = (TransformationMatrix x11 x12 x13 y11 x21 x22 x23 y21 x31 x32 x33 y31 x41 x42 x43 x44)

translate :: TransformationMatrix -> V3.Vector3 Number -> TransformationMatrix
translate
  (TransformationMatrix x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
  (V3.Vector3 y11 y21 y31) = (TransformationMatrix x11 x12 x13 (x14 + y11) x21 x22 x23 (x24 + y21) x31 x32 x33 (x34 + y31) x41 x42 x43 x44)

scale :: Number -> TransformationMatrix -> TransformationMatrix
scale
  m
  ( TransformationMatrix
      x11
      x12
      x13
      x14
      x21
      x22
      x23
      x24
      x31
      x32
      x33
      x34
      _
      _
      _
      _
  ) =
  ( TransformationMatrix
      (m * x11)
      x12
      x13
      x14
      x21
      (m * x22)
      x23
      x24
      x31
      x32
      (m * x33)
      x34
      (0.0)
      (0.0)
      (0.0)
      (1.0)
  )

rotateAboutAxisAtPoint :: TransformationMatrix -> Number -> Axis -> TransformationMatrix
rotateAboutAxisAtPoint m theta axis = case axis of
  Xaxis y z -> rotateMatrixAboutXAxisAtPoint m theta (Tuple y z)
  Yaxis x z -> rotateMatrixAboutYAxisAtPoint m theta (Tuple x z)
  Zaxis x y -> rotateMatrixAboutZAxisAtPoint m theta (Tuple x y)

rotateMatrixXAxis
  :: TransformationMatrix
  -> Radians
  -> TransformationMatrix
rotateMatrixXAxis matrix radians = applyRotationMatrix matrix (xAxisRotation radians)

rotateMatrixYAxis
  :: TransformationMatrix
  -> Radians
  -> TransformationMatrix
rotateMatrixYAxis matrix radians = applyRotationMatrix matrix (yAxisRotation radians)

rotateMatrixZAxis
  :: TransformationMatrix
  -> Radians
  -> TransformationMatrix
rotateMatrixZAxis matrix radians = applyRotationMatrix matrix (zAxisRotation radians)

rotateMatrixAboutXAxisAtPoint
  :: TransformationMatrix
  -> Number
  -> Tuple Y Z
  -> TransformationMatrix
rotateMatrixAboutXAxisAtPoint matrix degrees (Tuple (Y y) (Z z)) = offsetMatrix
  where
  radians@(Radians theta) = degreeToRadians degrees
  rotatedMatrix = rotateMatrixXAxis matrix radians
  offset = V3.Vector3
    0.0
    ((-y) * (cos theta) + z * (sin theta) + y)
    ((-y) * (sin theta) + (-z) * (cos theta) + z)
  offsetMatrix = setPosition rotatedMatrix offset

rotateMatrixAboutYAxisAtPoint
  :: TransformationMatrix
  -> Number
  -> Tuple X Z
  -> TransformationMatrix
rotateMatrixAboutYAxisAtPoint matrix degrees (Tuple (X x) (Z z)) = offsetMatrix
  where
  radians@(Radians theta) = degreeToRadians degrees
  rotatedMatrix = rotateMatrixYAxis matrix radians
  offset = V3.Vector3
    ((-x) * (cos theta) + z * (sin theta) + x)
    0.0
    ((-x) * (sin theta) + (-z) * (cos theta) + z)
  offsetMatrix = setPosition rotatedMatrix offset

rotateMatrixAboutZAxisAtPoint
  :: TransformationMatrix
  -> Number
  -> Tuple X Y
  -> TransformationMatrix
rotateMatrixAboutZAxisAtPoint matrix degrees (Tuple (X x) (Y y)) = offsetMatrix
  where
  radians@(Radians theta) = degreeToRadians degrees
  rotatedMatrix = rotateMatrixZAxis matrix radians
  offset = V3.Vector3
    ((-x) * (cos theta) + y * (sin theta) + x)
    ((-x) * (sin theta) + (-y) * (cos theta) + y)
    0.0
  offsetMatrix = setPosition rotatedMatrix offset

identityT :: TransformationMatrix
identityT = TransformationMatrix 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0

applyRotationMatrix
  :: TransformationMatrix
  -> RotationMatrix
  -> TransformationMatrix
applyRotationMatrix
  (TransformationMatrix _ _ _ x14 _ _ _ x24 _ _ _ x34 x41 x42 x43 x44)
  (RotationMatrix x11 x12 x13 x21 x22 x23 x31 x32 x33) = (TransformationMatrix x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)

getXColumn
  :: TransformationMatrix
  -> V3.Vector3 Number
getXColumn (TransformationMatrix x11 _ _ _ x21 _ _ _ x31 _ _ _ _ _ _ _) = V3.Vector3 x11 x21 x31

getYColumn
  :: TransformationMatrix
  -> V3.Vector3 Number
getYColumn (TransformationMatrix _ x12 _ _ _ x22 _ _ _ x32 _ _ _ _ _ _) = V3.Vector3 x12 x22 x32

getZColumn
  :: TransformationMatrix
  -> V3.Vector3 Number
getZColumn (TransformationMatrix _ _ x13 _ _ _ x23 _ _ _ x33 _ _ _ _ _) = V3.Vector3 x13 x23 x33

-- https://github.com/mrdoob/three.js/blob/4503ef10b81a00f5c6c64fe9a856881ee31fe6a3/src/math/Matrix4.js#L484
invert
  :: TransformationMatrix
  -> Either DivisionError TransformationMatrix
invert (TransformationMatrix x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44) = do
  let
    t11 = x23 * x34 * x42 - x24 * x33 * x42 + x24 * x32 * x43 - x22 * x34 * x43 - x23 * x32 * x44 + x22 * x33 * x44
    t12 = x14 * x33 * x42 - x13 * x34 * x42 - x14 * x32 * x43 + x12 * x34 * x43 + x13 * x32 * x44 - x12 * x33 * x44
    t13 = x13 * x24 * x42 - x14 * x23 * x42 + x14 * x22 * x43 - x12 * x24 * x43 - x13 * x22 * x44 + x12 * x23 * x44
    t14 = x14 * x23 * x32 - x13 * x24 * x32 - x14 * x22 * x33 + x12 * x24 * x33 + x13 * x22 * x34 - x12 * x23 * x34
    determinant = x11 * t11 + x21 * t12 + x31 * t13 + x41 * t14
  inverseDeterminant <- divide 1.0 determinant
  let
    x11' = t11 * inverseDeterminant
    x12' = t12 * inverseDeterminant
    x13' = t13 * inverseDeterminant
    x14' = t14 * inverseDeterminant

    x21' = (x24 * x33 * x41 - x23 * x34 * x41 - x24 * x31 * x43 + x21 * x34 * x43 + x23 * x31 * x44 - x21 * x33 * x44) * inverseDeterminant
    x22' = (x13 * x34 * x41 - x14 * x33 * x41 + x14 * x31 * x43 - x11 * x34 * x43 - x13 * x31 * x44 + x11 * x33 * x44) * inverseDeterminant
    x23' = (x14 * x23 * x41 - x13 * x24 * x41 - x14 * x21 * x43 + x11 * x24 * x43 + x13 * x21 * x44 - x11 * x23 * x44) * inverseDeterminant
    x24' = (x13 * x24 * x31 - x14 * x23 * x31 + x14 * x21 * x33 - x11 * x24 * x33 - x13 * x21 * x34 + x11 * x23 * x34) * inverseDeterminant

    x31' = (x22 * x34 * x41 - x24 * x32 * x41 + x24 * x31 * x42 - x21 * x34 * x42 - x22 * x31 * x44 + x21 * x32 * x44) * inverseDeterminant
    x32' = (x14 * x32 * x41 - x12 * x34 * x41 - x14 * x31 * x42 + x11 * x34 * x42 + x12 * x31 * x44 - x11 * x32 * x44) * inverseDeterminant
    x33' = (x12 * x24 * x41 - x14 * x22 * x41 + x14 * x21 * x42 - x11 * x24 * x42 - x12 * x21 * x44 + x11 * x22 * x44) * inverseDeterminant
    x34' = (x14 * x22 * x31 - x12 * x24 * x31 - x14 * x21 * x32 + x11 * x24 * x32 + x12 * x21 * x34 - x11 * x22 * x34) * inverseDeterminant

    x41' = (x23 * x32 * x41 - x22 * x33 * x41 - x23 * x31 * x42 + x21 * x33 * x42 + x22 * x31 * x43 - x21 * x32 * x43) * inverseDeterminant
    x42' = (x12 * x33 * x41 - x13 * x32 * x41 + x13 * x31 * x42 - x11 * x33 * x42 - x12 * x31 * x43 + x11 * x32 * x43) * inverseDeterminant
    x43' = (x13 * x22 * x41 - x12 * x23 * x41 - x13 * x21 * x42 + x11 * x23 * x42 + x12 * x21 * x43 - x11 * x22 * x43) * inverseDeterminant
    x44' = (x12 * x23 * x31 - x13 * x22 * x31 + x13 * x21 * x32 - x11 * x23 * x32 - x12 * x21 * x33 + x11 * x22 * x33) * inverseDeterminant
  pure $ TransformationMatrix x11' x12' x13' x14' x21' x22' x23' x24' x31' x32' x33' x34' x41' x42' x43' x44'

-- https://github.com/mrdoob/three.js/blob/4503ef10b81a00f5c6c64fe9a856881ee31fe6a3/src/math/Vector3.js#L237
applyMatrix4
  :: TransformationMatrix
  -> V3.Vector3 Number
  -> Either DivisionError (V3.Vector3 Number)
applyMatrix4
  (TransformationMatrix x11 x12 x13 x14 x21 x22 x23 x24 x31 x32 x33 x34 x41 x42 x43 x44)
  (V3.Vector3 vx vy vz) = do
  w <- divide 1.0 $ (x41 * vx) + (x42 * vy) + (x43 * vz) + x44
  let
    x = ((x11 * vx) + (x12 * vy) + (x13 * vz) + x14) * w
    y = ((x21 * vx) + (x22 * vy) + (x23 * vz) + x24) * w
    z = ((x31 * vx) + (x32 * vy) + (x33 * vz) + x34) * w
  pure $ V3.Vector3 x y z

multiply
  :: TransformationMatrix
  -> TransformationMatrix
  -> TransformationMatrix
multiply
  ( TransformationMatrix
      x11
      x12
      x13
      x14
      x21
      x22
      x23
      x24
      x31
      x32
      x33
      x34
      _
      _
      _
      _
  )
  ( TransformationMatrix
      y11
      y12
      y13
      y14
      y21
      y22
      y23
      y24
      y31
      y32
      y33
      y34
      _
      _
      _
      _
  ) = TransformationMatrix
  (x11 * y11 + x12 * y21 + x13 * y31)
  (x11 * y12 + x12 * y22 + x13 * y32)
  (x11 * y13 + x12 * y23 + x13 * y33)
  (x11 * y14 + x12 * y24 + x13 * y34 + x14)
  (x21 * y11 + x22 * y21 + x23 * y31)
  (x21 * y12 + x22 * y22 + x23 * y32)
  (x21 * y13 + x22 * y23 + x23 * y33)
  (x21 * y14 + x22 * y24 + x23 * y34 + x24)
  (x31 * y11 + x32 * y21 + x33 * y31)
  (x31 * y12 + x32 * y22 + x33 * y32)
  (x31 * y13 + x32 * y23 + x33 * y33)
  (x31 * y14 + x32 * y24 + x33 * y34 + x34)
  (0.0)
  (0.0)
  (0.0)
  (1.0)

inverseAfterTranslation
  :: V3.Vector3 Number
  -> TransformationMatrix
  -> TransformationMatrix
inverseAfterTranslation translation inverse = multiply inverse identityWithReverseTranslation
  where
  reverseTranslation = V3.multiplyByScalar (-1.0) translation
  identityWithReverseTranslation = setPosition identityT reverseTranslation