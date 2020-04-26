+++
title = "Writing a discord library using Polysemy"
author = ["Ben Simms"]
date = 2020-04-24
tags = ["programming", "haskell", "polysemy", "free-monads"]
draft = false
+++

Recently I've migrated my [discord library](https://github.com/nitros12/calamity) from mtl/transformers to [polysemy](https://github.com/isovector/polysemy)
after reading as many blog posts as I could find on it. My main reasons for
wanting to migrate were escaping from having to write newtypes and all N
instances every time I had a more than one effect in my stack, and how little
boilerplate polysemy requires to write new effects.

In this[^fn:1] and some upcoming blog post I'll be writing about the challenges I
faced and solved[^fn:2] while going about the conversion.


## Logging {#logging}

The first effect that I converted from mtl to Polysemy was logging, originally I
was using [simple-log](https://hackage.haskell.org/package/simple-log) because I liked being able have areas of code run inside
logging 'scopes', at the time [co-log-polysemy](https://hackage.haskell.org/package/co-log-polysemy) was the only existing logging
framework for polysemy and I was planning to use it, but instead I found [di](https://hackage.haskell.org/package/di) and
decided to write a [Polysemy effect for it](https://github.com/nitros12/di-polysemy).

The effect definition is the following:

```haskell
data Di level path msg m a where
  Log    :: level -> msg -> Di level path msg m ()
  Flush  :: Di level path msg m ()
  Push   :: D.Segment -> m a -> Di level D.Path msg m a
  Attr_  :: D.Key -> D.Value -> m a -> Di level D.Path msg m a
```

I went on to write an interpreter making use of the existing framework in Di for
printing out the log, which I found simple to write as it mostly consisted of
playing jigsaw with types:

```haskell
go :: Member (Embed IO) r0 => DC.Di level D.Path msg -> Sem (Di level D.Path msg ': r0) a0 -> Sem r0 a0
go di m = (`interpretH` m) $ \case
  Log level msg -> do
    t <- embed @IO $ DC.log di level msg
    pureT t
  Flush         -> do
    t <- embed @IO $ DC.flush di
    pureT t
  Push s m'     -> do
    mm <- runT m'
    raise $ go (Df1.push s di) mm
  Attr_ k v m'  -> do
    mm <- runT m'
    raise $ go (Df1.attr_ k v di) mm
```

The handlers for `Log` and `Flush` are simple enough, just embed the IO action
and wrap the result, and the handlers for `Push` and `Attr` consist of running
the nested action with the modified logger state, this is pretty much `Reader`
and I could probably rewrite this to just reinterpret the `Di` effect in terms
of `Reader`.

However this interpreter needs to get a [`Di.Core.Di`](https://hackage.haskell.org/package/di-core-1.0.4/docs/Di-Core.html#t:Di) from somewhere, and the
only place to do that[^fn:3] is to use [Di.Core.new](https://hackage.haskell.org/package/di-core-1.0.4/docs/Di-Core.html#v:new) which has the signature:

```haskell
new
  :: forall m level path msg a
  .  (MonadIO m, Ex.MonadMask m)
  => (Log level path msg -> IO ())
  -> (Di level path msg -> m a)
  -> m a
```

That [`MonadMask`](https://hackage.haskell.org/package/exceptions-0.10.0/docs/Control-Monad-Catch.html#t:MonadMask) constraint means that we can't just use polysemy's `Sem r`
monad, my first resolution to this was to [copy the source of `new`](https://github.com/nitros12/di-polysemy/blob/863cc0072d846b1d96eca6467bc836bd098f7bb7/src/DiPolysemy.hs#L68-L124) and replace
[`Control.Exception.Safe.finally`](http://hackage.haskell.org/package/safe-exceptions-0.1.7.0/docs/Control-Exception-Safe.html#v:finally) with polysemy's [`Resource.finally`](https://hackage.haskell.org/package/polysemy-1.3.0.0/docs/Polysemy-Resource.html#v:finally)[^fn:4]

This way required too much hackery for my liking, so I spent some time
figuring out how to lower a `Member (Embed IO) r => Sem r a` to `IO a`, and
luckily the [`Resource`](https://hackage.haskell.org/package/polysemy-1.3.0.0/docs/src/Polysemy.Resource.html#resourceToIO) effect does pretty much what I want to do already, so my
current solution is to create a higher order effect with a single operation:

```haskell
data DiIOInner m a where
  RunDiIOInner :: (DC.Log level D.Path msg -> IO ()) -> (DC.Di level D.Path msg -> m a) -> DiIOInner m a
```

And define an interpreter:

```haskell
diToIO :: forall r a. Member (Embed IO) r => Sem (DiIOInner ': r) a -> Sem r a
diToIO = interpretH
  (\case RunDiIOInner commit a -> do
           istate <- getInitialStateT
           ma <- bindT a

           withLowerToIO $ \lower finish -> do
             let done :: Sem (DiIOInner ': r) x -> IO x
                 done = lower . raise . diToIO

             DC.new commit (\di -> do
                               res <- done (ma $ istate $> di)
                               finish
                               pure res))
```

This effect is only ever used internally in the implementation of `runDiToIO`:

```haskell
runDiToIO
  :: forall r level msg a.
  Member (Embed IO) r
  => (DC.Log level D.Path msg -> IO ())
  -> Sem (Di level D.Path msg ': r) a
  -> Sem r a
runDiToIO commit m = diToIO $ runDiIOInner commit (`go` raiseUnder m)
  where
    go :: -- ...
```

I'm not sure if this is the best way to perform the ritual of lowering the Sem
monad to IO, but I can't see any way to perform it without having the ad-hoc
effect.

Anyway, after writing the interpreter, the helper functions can be written,
they're fairly repetitive so I'll only include the first few:

```haskell
runDiToStderrIO :: Member (Embed IO) r => Sem (Di D.Level D.Path D.Message ': r) a -> Sem r a
runDiToStderrIO m = do
  commit <- embed @IO $ DH.stderr Df1.df1
  runDiToIO commit m

attr :: forall value level msg r a. (D.ToValue value, Member (Di level D.Path msg) r) => D.Key -> value -> Sem r a -> Sem r a
attr k v = attr_ @level @msg k (D.value v)

debug :: forall msg path r. (D.ToMessage msg, Member (Di D.Level path D.Message) r) => msg -> Sem r ()
debug = log @D.Level @path D.Debug . D.message

info :: forall msg path r. (D.ToMessage msg, Member (Di D.Level path D.Message) r) => msg -> Sem r ()
info = log @D.Level @path D.Info . D.message
```

The manual type applications would normally not be necessary if you were to use
[`Polysemy.Plugin`](https://hackage.haskell.org/package/polysemy-plugin), but haddock currently (GHC 8.6.5) dies when it tries to build
docs with the plugin enabled.


### Usage {#usage}

Now that the logger effect is written, we can use it like so:

```haskell
import qualified Df1
import           DiPolysemy
import           Polysemy
import           Prelude                     hiding ( error )

main :: IO ()
main = runM . runDiToStderrIO $ logTest

logTest :: Member (Di Df1.Level Df1.Path Df1.Message) r => Sem r ()
logTest = do
  info_ "hello"
  notice_ "this is a notice"
  push "some-scope" $ do
    warning_ "this is inside a scope"
    attr "x" (4 :: Int) $ do
      debug_ "this one has an attribute"
  emergency_ "and we're done"
```

Which produces the following:

```nil
2020-04-25T03:59:44.452126488Z INFO hello
2020-04-25T03:59:44.452136280Z NOTICE this is a notice
2020-04-25T03:59:44.452147183Z /some-scope WARNING this is inside a scope
2020-04-25T03:59:44.452156206Z /some-scope x=4 DEBUG this one has an attribute
2020-04-25T03:59:44.452162458Z EMERGENCY and we're done
```

[^fn:1]: This blog post was sponsored by [theophile.choutri.eu/microfund](https://theophile.choutri.eu/microfund.html)
[^fn:2]: although some of my solutions I feel aren't the best, and I'd love to be made aware of any alternate solutions
[^fn:3]: Without writing my own logger
[^fn:4]: Though this implementation probably doesn't respect async exceptions correctly in some way.
