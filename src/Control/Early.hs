-- | Early return in monads.

module Control.Early
  ( Early(..)
  , early
  , earlyThen
  ) where

--------------------------------------------------------------------------------
-- Class providing different early return types

-- | A class for things which can offer branching for early return.
--
-- The most obvious two types are 'Either' and 'Maybe'.
class Functor f => Early f where
  dispatch :: Applicative m => f a -> (a -> m (f b)) -> m (f b)

--------------------------------------------------------------------------------
-- Top-level API

-- | Early return specialized on f (for now).
early :: (Monad m, Early f) => m (f a) -> (a -> m (f b)) -> m (f b)
early m f = do
  r <- m
  dispatch r f
{-# INLINE early #-}

-- | Early return specialized on f (for now).
earlyThen :: (Monad m, Early f) => m (f a) -> m (f b) -> m (f b)
earlyThen m f = early m (const f)
{-# INLINE earlyThen #-}

--------------------------------------------------------------------------------
-- Instances

instance Early (Either e) where
  dispatch r f =
    case r of
      Left e -> pure (Left e)
      Right x -> f x
  {-# INLINE dispatch #-}

instance Early Maybe where
  dispatch r f =
    case r of
      Nothing -> pure Nothing
      Just x -> f x
  {-# INLINE dispatch #-}
