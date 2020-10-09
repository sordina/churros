# Churros

> Chan + Arrow = Churro

Simple alternative to Conduit, Pipes, Streams, Machines, etc.

Use-case is CSP like scenarios where you need a graph of actors.

Advantages over other alternatives:

* Focus on IO processes
* Dynamic choice of consumption/production
* Arrow instance
* Choice of transport via the `Transport` class

See [Hackage](https://hackage.haskell.org/package/churros-0.1.0.0/candidate) for more info!

## Examples

See `./test/` directory for more extensive examples.

```haskell
import Control.Churro

main = do
   runWaitChan        $ sourceList [1..10] >>> processDebug "after source" >>> delay 1 {- seconds -} >>> arr succ >>> sinkPrint
   wait =<< run @Chan $ sourceIO (\cb -> cb 1 >> print "Doing whatever!" >> cb 5) >>> filterC (> 3) >>> sinkIO print
```

## Testing

Cabal test-suite including doctests:

> cabal exec cabal test

## TODO

* [x] Recovery/Retry capability
* [x] Fix await deadlock
* [x] Generic Chan functions, then specific newtype
* [x] Stop using list functions
* [x] Different transport options, buffered, etc.
* [x] Write doctests for functionality
* [ ] Get Doctests working as part of the cabal test-suite
* [ ] Get haddocks rendering correctly - Including contents
* [ ] Different transports for sections of the graph
* [ ] Allow configurable parallelism
* [ ] Early termination if downstream consumer completes
    - [ ] Ensure that infinite lists work when partially consumed