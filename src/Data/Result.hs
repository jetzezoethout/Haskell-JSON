module Data.Result
  ( pattern Success
  , pattern Failure
  , parallel2
  , parallel
  , parallelTraverse
  , mapErrors
  , single
  , Result
  ) where

import           Data.List.NonEmpty (NonEmpty, singleton, toList)

newtype Result e a =
  Result (Either (NonEmpty e) a)
  deriving newtype (Functor, Applicative, Monad)

pattern Success :: a -> Result e a
pattern Success value = Result (Right value)

pattern Failure :: NonEmpty e -> Result e a
pattern Failure errors = Result (Left errors)

{-# COMPLETE Success, Failure #-}
instance (Show a, Show e) => Show (Result e a) where
  show :: (Show a, Show e) => Result e a -> String
  show (Failure errors) = "Failure " <> show (toList errors)
  show (Success value)  = "Success " <> show value

parallel2 :: (a -> b -> c) -> Result e a -> Result e b -> Result e c
parallel2 _ (Failure errors1) (Failure errors2) = Failure $ errors1 <> errors2
parallel2 _ (Failure errors) _                  = Failure errors
parallel2 _ _ (Failure errors)                  = Failure errors
parallel2 f (Success x) (Success y)             = Success $ f x y

parallel :: [Result e a] -> Result e [a]
parallel = foldr (parallel2 (:)) (Success [])

parallelTraverse :: (a -> Result e b) -> [a] -> Result e [b]
parallelTraverse f = parallel . fmap f

single :: e -> Result e a
single = Failure . singleton

mapErrors :: (e1 -> e2) -> Result e1 a -> Result e2 a
mapErrors f (Failure errors) = Failure $ fmap f errors
mapErrors _ (Success value)  = Success value
