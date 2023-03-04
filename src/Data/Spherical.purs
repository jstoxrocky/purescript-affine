module Data.Affine.Spherical where

import Prelude
import Data.Affine.Vectors.Vector3 as V3
import Data.Number (sin, cos, atan2, acos)
import Data.Either (Either)
import Data.Affine.DivisionError (DivisionError)
import Data.Affine.Division (divide)

data Spherical = Spherical Number SphericalAngles

data SphericalAngles = SphericalAngles Number Number

sphericalAnglesToVector3
  :: Spherical
  -> V3.Vector3 Number
sphericalAnglesToVector3 (Spherical radius (SphericalAngles theta phi)) = V3.Vector3 x y z
  where
  sinPhiRadius = (sin phi) * radius
  x = sinPhiRadius * (sin theta)
  y = (cos phi) * radius
  z = sinPhiRadius * (cos theta)

vector3ToSpherical
  :: V3.Vector3 Number
  -> Either DivisionError Spherical
vector3ToSpherical offset@(V3.Vector3 x y z) = do
  let
    radius = V3.length offset
    theta = atan2 x z
  phi <- acos <$> divide y radius
  pure $ Spherical radius $ SphericalAngles theta phi