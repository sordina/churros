# Churros

[![Github Actions](https://github.com/sordina/churros/actions/workflows/test.yml/badge.svg)](https://github.com/sordina/churros/actions/workflows/test.yml)

> Chan + Arrow = Churro

Simple alternative to
   [Conduit](https://hackage.haskell.org/package/conduit),
   [Pipes](https://hackage.haskell.org/package/pipes),
   [Streaming](https://hackage.haskell.org/package/streaming),
   [Machines](https://hackage.haskell.org/package/machines), etc.

Use-case is CSP like scenarios where you need a graph of actors.

Signup Example:

```
New-Users -->  Validate -------?----->  Deploy  -----?--------------> Log
Subscribe    REST Request      |      Run Server     |         Write File
{-> ID}    {ID -> (ID,Email}   |    {Email -> Port}  |  {Port|Receipt ->}
                               |                     |
                               +-------> Warn -------+
                                      Send Email
                                  {Email -> Receipt}
```

Developed from a history of attempting to use co-routines libraries for setting up complicated asynchronous processes
such as collections of MIDI instruments, etc, but being frustrated by not easily being able to conditionally
consume and emit events. In these situations I'd fall back on creating a bunch of Chans and piping events manually.
Churros just formalises that strategy to help you get it right!

Advantages over other alternatives:

* Focus on IO processes
* Dynamic choice of consumption/production
* Arrow instance
* Choice of transport via the `Transport` class

Disadvantages:

* No pure interface!
* Type for the async action restricted to Monoid for most operations.
* Limited ability to perform lock-step computation (although this is by design)

See [Hackage](https://hackage.haskell.org/package/churros) for more info!

## Examples

See `./test/` directory for more extensive examples.

```haskell
import Control.Churro

main = do
   runWaitChan             $ sourceList [1..10] >>> processDebug "after source" >>> delay 1 {- seconds -} >>> arr succ >>> sinkPrint
   (wait =<<)  $ run @Chan $ sourceIO (\cb -> cb 1 >> print "Doing whatever!" >> cb 5) >>> filterC (> 3) >>> sinkIO print
```

## Testing

Cabal test-suite including doctests:

> cabal exec cabal test

Or for itterative development:

> find {src,test} | entr -- cabal exec -- doctest -isrc -itest test/Churro/Test/Examples.hs

## TODO

* [ ] Add seperate projects to allow minimal core and enciched ecosystem e.g. churros-unagi
* [ ] Create profunctor instance
* [ ] Create contravariant functor instance
* [ ] Create ArrowChoice instance
* [ ] Create ArrowLoop instance
* [ ] Different transports for sections of the graph
* [ ] Allow configurable parallelism / Pool Churro functions
* [x] Recovery/Retry capability
* [x] Fix await deadlock
* [x] Generic Chan functions, then specific newtype
* [x] Stop using list functions
* [x] Different transport options, buffered, etc.
* [x] Write doctests for functionality
* [x] Get Doctests working as part of the cabal test-suite
* [x] Bundle in/out channels in the Transport class to allow Unagi to implement it
* [x] Allow returning of results from run functions
* [x] Get haddocks rendering correctly - Including contents
* [x] Early termination if downstream consumer completes
    - [x] Ensure that infinite lists work when partially consumed
