# Churros

> Chan + Arrow = Churro

Simple alternative to Conduit, Pipes, Streams, Machines, etc.

Use-case is CSP like scenarios where you need a graph of actors.

Advantages over other alternatives:

* Focus on IO processes
* Dynamic choice of consumption/production
* Arrow instance
* Choice of transport via the `Transport` class

## Examples

See `./test/` directory for more extensive examples.

```haskell
import Control.Churro

main = do
   runWaitChan        $ sourceList [1..10] >>> processDebug "after source" >>> delay 1 {- seconds -} >>> arr succ >>> sinkPrint
   wait =<< run @Chan $ sourceIO (\cb -> cb 1 >> print "Doing whatever!" >> cb 5) >>> filterC (> 3) >>> sinkIO print
```