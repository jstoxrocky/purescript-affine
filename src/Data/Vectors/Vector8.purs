module Affine.Data.Vectors.Vector8 where

import Prelude
import Data.Traversable (class Traversable)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Array.NonEmpty as NE
import Affine.Data.Vectors.Vector7 (Vector7(..))
import Data.Tuple (Tuple(..))

data Vector8 a = Vector8 a a a a a a a a

derive instance genericVector8 :: Generic (Vector8 a) _

instance showVector8 :: Show a => Show (Vector8 a) where
  show = genericShow

derive instance eqVector8 :: Eq a => Eq (Vector8 a)

instance functorVector8 :: Functor Vector8 where
  map f (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) =
    Vector8
      (f x1)
      (f x2)
      (f x3)
      (f x4)
      (f x5)
      (f x6)
      (f x7)
      (f x8)

instance foldableVector8 :: Foldable Vector8 where
  foldr f e (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = foldr f e [ x1, x2, x3, x4, x5, x6, x7, x8 ]
  foldl f e (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = foldl f e [ x1, x2, x3, x4, x5, x6, x7, x8 ]
  foldMap f (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = foldMap f [ x1, x2, x3, x4, x5, x6, x7, x8 ]

instance traversableVector8 :: Traversable Vector8 where
  traverse f (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = Vector8
    <$> (f x1)
    <*> (f x2)
    <*> (f x3)
    <*> (f x4)
    <*> (f x5)
    <*> (f x6)
    <*> (f x7)
    <*> (f x8)
  sequence (Vector8 m1 m2 m3 m4 m5 m6 m7 m8) = Vector8
    <$> m1
    <*> m2
    <*> m3
    <*> m4
    <*> m5
    <*> m6
    <*> m7
    <*> m8

toNonEmptyArray :: forall a. Vector8 a -> NE.NonEmptyArray a
toNonEmptyArray (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = NE.cons' x1 [ x2, x3, x4, x5, x6, x7, x8 ]

toArray :: forall a. Vector8 a -> Array a
toArray (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = [ x1, x2, x3, x4, x5, x6, x7, x8 ]

uncons
  :: forall a
   . Vector8 a
  -> Tuple a (Vector7 a)
uncons (Vector8 x1 x2 x3 x4 x5 x6 x7 x8) = Tuple x1 (Vector7 x2 x3 x4 x5 x6 x7 x8)

cons'
  :: forall a
   . a
  -> Vector7 a
  -> Vector8 a
cons' x0 (Vector7 x1 x2 x3 x4 x5 x6 x7) = Vector8 x0 x1 x2 x3 x4 x5 x6 x7

head :: forall a. Vector8 a -> a
head (Vector8 x1 _ _ _ _ _ _ _) = x1
