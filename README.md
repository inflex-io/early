# early

Add early return to any monad

## Description

This package is a GHC plugin to add special syntax for early return in
`do`-notation. It provides a way to terminate the current monad with a
result, usually a failure result, but not necessarily.

It should not be confused with an exception handler. It uses regular
values everywhere.

The plugin is enabled in any module via a pragma.

``` haskell
{-# OPTIONS -F -pgmF=early #-}
```

The syntax `?` can be added to the end of any `do` statement to make
it short-circuit when the action returns a `Left`.

Suppose that `grabEnv :: String -> IO (Either Error String)`, then you
can write this:

```haskell
app :: IO (Either Error String)
app = do
  path <- grabEnv "PATH"?
  magic <- grabEnv "MAGIC"?
  pure (Right (path ++ magic))
```

Note the final `pure` in the do should wrap the type, as the type of
the whole `do`-block has changed.

That's it! See `test/Main.hs` for full example.

## Details

The syntax `stmt?` is desugared in this way:

* `do stmt?; next` becomes `do earlyThen stmt next`
* `do pat <- stmt?; next; next2` becomes `do early stmt (\pat -> do next; next2; ...)`

The `early` and `earlyThen` are driven by the `Early` class, which any
functor-like data type can implement.

``` haskell
class Functor f => Early f where
  dispatch :: Applicative m => f a -> (a -> m (f b)) -> m (f b)
```

Two provided instances out of the box are `Either e` and `Maybe`.

## Inspiration

The syntax and concept of using simple return values for early
termination and failure handling is inspired
[by Rust's error handling](https://doc.rust-lang.org/rust-by-example/error/result/enter_question_mark.html).

## Special thanks

The following people's work helped me to get my work done faster.

* Oleg Grenrus https://github.com/phadej/idioms-plugins
* Mark Karpov https://github.com/mrkkrp/ghc-syntax-highlighter
