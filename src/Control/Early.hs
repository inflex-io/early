-- | Early return in monads.

module Control.Early
  ( early
  , earlyThen
  ) where

-- | Early return specialized on Either (for now).
early :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
early m f = do
  r <- m
  case r of
    Left e -> pure (Left e)
    Right x -> f x
{-# INLINE early #-}

-- | Early return specialized on Either (for now).
earlyThen :: Monad m => m (Either e a) -> m (Either e b) -> m (Either e b)
earlyThen m f = early m (const f)
{-# INLINE earlyThen #-}

--
