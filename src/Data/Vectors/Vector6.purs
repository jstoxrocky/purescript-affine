module Data.Affine.Vectors.Vector6 where

import Prelude
import Data.Traversable (class Traversable)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Array.NonEmpty (NonEmptyArray, cons')

data Vector6 a = Vector6 a a a a a a

derive instance genericVector6 :: Generic (Vector6 a) _

instance showVector6 :: Show a => Show (Vector6 a) where
  show = genericShow

derive instance eqVector6 :: Eq a => Eq (Vector6 a)

instance functorVector6 :: Functor Vector6 where
  map f (Vector6 x1 x2 x3 x4 x5 x6) =
    Vector6
      (f x1)
      (f x2)
      (f x3)
      (f x4)
      (f x5)
      (f x6)

instance foldableVector6 :: Foldable Vector6 where
  foldr f e (Vector6 x1 x2 x3 x4 x5 x6) = foldr f e [ x1, x2, x3, x4, x5, x6 ]
  foldl f e (Vector6 x1 x2 x3 x4 x5 x6) = foldl f e [ x1, x2, x3, x4, x5, x6 ]
  foldMap f (Vector6 x1 x2 x3 x4 x5 x6) = foldMap f [ x1, x2, x3, x4, x5, x6 ]

instance traversableVector6 :: Traversable Vector6 where
  traverse f (Vector6 x1 x2 x3 x4 x5 x6) = Vector6
    <$> (f x1)
    <*> (f x2)
    <*> (f x3)
    <*> (f x4)
    <*> (f x5)
    <*> (f x6)
  sequence (Vector6 m1 m2 m3 m4 m5 m6) = Vector6
    <$> m1
    <*> m2
    <*> m3
    <*> m4
    <*> m5
    <*> m6

nonEmptyArray :: forall a. Vector6 a -> NonEmptyArray a
nonEmptyArray (Vector6 x1 x2 x3 x4 x5 x6) = cons' x1 [ x2, x3, x4, x5, x6 ]

toArray :: forall a. Vector6 a -> Array a
toArray (Vector6 x1 x2 x3 x4 x5 x6) = [ x1, x2, x3, x4, x5, x6 ]
