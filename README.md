# early

Add early return to any monad

## Description

This package is a GHC plugin to add special syntax for early return in
`do`-notation.

The plugin is enabled in any module via,

``` haskell
{-# OPTIONS -F -pgmF=early #-}
```

The syntax `?` can be added to the end of any `do` statement to make
it short-circuit when the action returns a `Left`.

```haskell
app :: IO (Either Error String)
app = do
  path <- grabEnv "PATH"?
  magic <- grabEnv "MAGIC"?
  pure (Right (path ++ magic))
```

That's it!

## Special thanks

The following people's work helped me to get my work done faster.

* Oleg Grenrus https://github.com/phadej/idioms-plugins
* Mark Karpov https://github.com/mrkkrp/ghc-syntax-highlighter
