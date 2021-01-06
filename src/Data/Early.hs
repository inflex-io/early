module Data.Early
  ( FoldableEarly(..)
  , TraversableEarly(..)
  ) where

import           Control.Early
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

class Foldable t => FoldableEarly t where
  foldE :: (Monad m, Early f, Applicative f)
        => (x -> a -> m (f x)) -> x -> t a -> m (f x)

instance FoldableEarly [] where
  foldE cons nil0 = go nil0
    where
      go nil [] = pure (pure nil)
      go nil (x:xs) = early (cons nil x) (\x' -> go x' xs)

instance FoldableEarly Seq where
  foldE cons nil0 = go nil0
    where
      go nil Seq.Empty = pure (pure nil)
      go nil (x Seq.:<| xs) = early (cons nil x) (\x' -> go x' xs)

class Traversable t => TraversableEarly t where
  traverseE :: (Monad m, Early f, Applicative f)
            => (a -> m (f b)) -> t a -> m (f (t b))

instance TraversableEarly [] where
  traverseE f = go []
    where
      go acc [] = pure (pure (reverse acc))
      go acc (x:xs) = early (f x) (\x' -> go (x' : acc) xs)

instance TraversableEarly Seq where
  traverseE f = go mempty
    where
      go acc Seq.Empty = pure (pure acc)
      go acc (x Seq.:<| xs) = early (f x) (\x' -> go (acc Seq.:|> x') xs)