# early

Add early return to any `do`-expression

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [early](#early)
    - [Description](#description)
    - [How it works](#how-it-works)
    - [Details](#details)
        - [Why not `ExceptT` or exceptions?](#why-not-exceptt-or-exceptions)
    - [Inspiration](#inspiration)
    - [Special thanks](#special-thanks)

<!-- markdown-toc end -->


## Description

This package is a GHC plugin to add special syntax for early return in
`do`-notation. It provides a way to terminate the current
`do`-expression with a result, usually a failure result, but not
necessarily. It should not be confused with an exception handler. It
uses regular values everywhere.

## How it works

The plugin is enabled in any module via a pragma.

``` haskell
{-# OPTIONS -F -pgmF=early #-}
```

The syntax `?` can be added to the end of any `do` statement to make
it short-circuit when the action produces a certain "stop" result
(such as `Left`, or `Nothing`; the particular type is type-class
based, see the Details section below).

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
early :: (Monad m, Early f) => m (f a) -> (a -> m (f b)) -> m (f b)
earlyThen :: (Monad m, Early f) => m (f a) -> m (f b) -> m (f b)
```

``` haskell
class Functor f => Early f where
  dispatch :: Applicative m => f a -> (a -> m (f b)) -> m (f b)
```

Two provided instances out of the box are `Either e` and `Maybe`, but
others can be added freely, such as a `Failure e a` type of your
library, etc.

### Why not `ExceptT` or exceptions?

Full explanation here:
[my recoverable errors post](https://chrisdone.com/posts/try-do/).

Because `ExceptT` (or `ContT`) cannot be an
instance of `MonadUnliftIO`. It is not unliftable; this means that
exceptions, cleanup and concurrency don't have an interpretation. This
is an area where monad transformers in `mtl`/`transformers` don't
compose. Other free monads commute, but then you have to use a free
monad which has a complicated story regarding performance.

## Inspiration

The syntax and concept of using simple return values for early
termination and failure handling is inspired
[by Rust's error handling](https://doc.rust-lang.org/rust-by-example/error/result/enter_question_mark.html). The
`Early` class resembles the
[Try trait](https://doc.rust-lang.org/std/ops/trait.Try.html), but it
slightly different, as Haskell has higher-kinded types.

Additionally, one can take a Rust-like view of error handling in
Haskell:

|Use-case|Haskell|Rust|
|---:|---:|
|Unrecoverable errors|Throwing exceptions|Panics|
|Recoverable errors|Return `Either`/`Maybe`|Return `Result`/`Some`|

This plugin allows one to structure their code in such a way.

## Future Work

A small library of short-circuiting `traverse`/`fold` would let one
use actions that return `Either`/`Maybe`.

## Special thanks

The following people's work helped me a lot to get my work done faster:

* Shayne Fletcher and Neil Mitchell https://github.com/digital-asset/ghc-lib
* Oleg Grenrus https://github.com/phadej/idioms-plugins
* Mark Karpov https://github.com/mrkkrp/ghc-syntax-highlighter
