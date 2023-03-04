module Data.Affine.Vectors.Vector7 where

import Prelude
import Data.Traversable (class Traversable)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Array.NonEmpty as NE
import Data.Tuple (Tuple(..))
import Data.Affine.Vectors.Vector6 (Vector6(..))

data Vector7 a = Vector7 a a a a a a a

derive instance genericVector7 :: Generic (Vector7 a) _

instance showVector7 :: Show a => Show (Vector7 a) where
  show = genericShow

derive instance eqVector7 :: Eq a => Eq (Vector7 a)

instance functorVector7 :: Functor Vector7 where
  map f (Vector7 x1 x2 x3 x4 x5 x6 x7) =
    Vector7
      (f x1)
      (f x2)
      (f x3)
      (f x4)
      (f x5)
      (f x6)
      (f x7)

instance foldableVector7 :: Foldable Vector7 where
  foldr f e (Vector7 x1 x2 x3 x4 x5 x6 x7) = foldr f e [ x1, x2, x3, x4, x5, x6, x7 ]
  foldl f e (Vector7 x1 x2 x3 x4 x5 x6 x7) = foldl f e [ x1, x2, x3, x4, x5, x6, x7 ]
  foldMap f (Vector7 x1 x2 x3 x4 x5 x6 x7) = foldMap f [ x1, x2, x3, x4, x5, x6, x7 ]

instance traversableVector7 :: Traversable Vector7 where
  traverse f (Vector7 x1 x2 x3 x4 x5 x6 x7) = Vector7
    <$> (f x1)
    <*> (f x2)
    <*> (f x3)
    <*> (f x4)
    <*> (f x5)
    <*> (f x6)
    <*> (f x7)
  sequence (Vector7 m1 m2 m3 m4 m5 m6 m7) = Vector7
    <$> m1
    <*> m2
    <*> m3
    <*> m4
    <*> m5
    <*> m6
    <*> m7

uncons
  :: forall a
   . Vector7 a
  -> Tuple a (Vector6 a)
uncons (Vector7 x1 x2 x3 x4 x5 x6 x7) = Tuple x1 (Vector6 x2 x3 x4 x5 x6 x7)

cons'
  :: forall a
   . a
  -> Vector6 a
  -> Vector7 a
cons' x0 (Vector6 x1 x2 x3 x4 x5 x6) = Vector7 x0 x1 x2 x3 x4 x5 x6

toNonEmptyArray :: forall a. Vector7 a -> NE.NonEmptyArray a
toNonEmptyArray (Vector7 x1 x2 x3 x4 x5 x6 x7) = NE.cons' x1 [ x2, x3, x4, x5, x6, x7 ]

toArray :: forall a. Vector7 a -> Array a
toArray (Vector7 x1 x2 x3 x4 x5 x6 x7) = [ x1, x2, x3, x4, x5, x6, x7 ]
