{-# OPTIONS -F -pgmF=early #-}

module Main (main) where

import Control.Early
import System.Environment

data Error =
  MissingEnv String
  deriving (Show)

grabEnv :: String -> IO (Either Error String)
grabEnv key = do
  result <- lookupEnv key
  pure (maybe (Left (MissingEnv key)) Right result)

main :: IO ()
main = do
  result <- app
  print result

app :: IO (Either Error String)
app = do
  path <- fmap (maybe (Left (MissingEnv "PATH")) Right) $ lookupEnv "PATH"?
  grabEnv "PWD"?
  magic <- grabEnv "PATH"?
  pure (Right (path ++ magic))
