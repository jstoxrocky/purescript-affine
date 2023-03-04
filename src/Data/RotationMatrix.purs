module Affine.Data.RotationMatrix where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Affine.Data.Vectors.Vector3 as V3
import Data.Either (Either)
import Affine.Data.DivisionError (DivisionError)
import Data.Number (sin, cos)
import Affine.Data.Rotation (Radians(..))

data RotationMatrix = RotationMatrix
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number
  Number

derive instance genericRotationMatrix :: Generic RotationMatrix _
instance showRotationMatrix :: Show RotationMatrix where
  show = genericShow

derive instance eqRotationMatrix :: Eq RotationMatrix

up :: V3.Vector3 Number
up = V3.Vector3 0.0 1.0 0.0

lookAtRotation
  :: V3.Vector3 Number
  -> V3.Vector3 Number
  -> Either DivisionError RotationMatrix
lookAtRotation eye target = do
  zAxis@(V3.Vector3 zAxisX zAxisY zAxisZ) <- V3.normalize $ (V3.subtract eye target)
  xAxis@(V3.Vector3 xAxisX xAxisY xAxisZ) <- V3.normalize $ (V3.crossProduct up zAxis)
  (V3.Vector3 yAxisX yAxisY yAxisZ) <- V3.normalize $ (V3.crossProduct zAxis xAxis)
  pure $ RotationMatrix
    xAxisX
    yAxisX
    zAxisX
    xAxisY
    yAxisY
    zAxisY
    xAxisZ
    yAxisZ
    zAxisZ

xAxisRotation
  :: Radians
  -> RotationMatrix
xAxisRotation (Radians radians) = RotationMatrix
  1.0
  0.0
  0.0
  0.0
  (cos radians)
  (-(sin radians))
  0.0
  (sin radians)
  (cos radians)

yAxisRotation
  :: Radians
  -> RotationMatrix
yAxisRotation (Radians radians) = RotationMatrix
  (cos radians)
  0.0
  (-(sin radians))
  0.0
  1.0
  0.0
  (sin radians)
  0.0
  (cos radians)

zAxisRotation
  :: Radians
  -> RotationMatrix
zAxisRotation (Radians radians) = RotationMatrix
  (cos radians)
  (-(sin radians))
  0.0
  (sin radians)
  (cos radians)
  0.0
  0.0
  0.0
  1.0