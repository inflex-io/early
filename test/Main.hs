{-# OPTIONS -F -pgmF=early #-}

module Main (main) where

import System.Environment

data Error = MissingEnv String

grabEnv :: String -> IO (Either Error String)
grabEnv key = do
  result <- lookupEnv key
  pure (maybe (Left (MissingEnv key)) Right result)

main :: IO ()
main = do result <- app
          print result

app :: IO (Either Error String)
app = do
  path <- grabEnv "PATH"?
  magic <- grabEnv "MAGIC"?
  pure (Right (path ++ magic))
