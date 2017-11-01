# simple

I based my code on [this tutorial for `distributed-process-simplelocalnet`](http://hackage.haskell.org/package/distributed-process-simplelocalnet-0.2.0.9/docs/Control-Distributed-Process-Backend-SimpleLocalnet.html). It's a simple system with a single master and multiple slaves.

The pdf says *"Several nodes continuously send messages to other nodes in such way, ..."* but my code just has one sender, the *master*, and multiple slaves.
The slaves accumulate all the numbers they receive, according the the formula in the pdf, until `--wait-for` seconds have elapsed.  Then each slave prints the tuple.

## Running it

This probably won't work as you expect with distributed nodes, based on the final comment on "2.3 Cluster configuration". I don't know enough about this networking system to understand that comment fully.
My system relies on "node discovery based on UDP multicast", according to the above tutorial, and I don't know what your network allows between multiple machines.

See `run.bash` for a bash script to run this. It loads a number of slaves, then the master. The master delays itself for 0.5 seconds to ensure the slaves have enough time to get going and be visible on the network.

## command line

The `--send-for` and `--wait-for` options are compulsory. (They're not "options" then I guess!)

    simple-exe --send-for 5 --wait-for 1

`--with-seed` specifies the seed, defaulting to 1337

## the code itself

I'm sure there are many ways this could be improved; perhaps some sort of `State`-like monads to track things like the state of the random number generator and to track the running totals. I tended to use recursion to manage state, I don't know how appropriate that is in general
