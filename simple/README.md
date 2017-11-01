# simple

Clarify what I did, as it is a bit less than was asked for.

I based my code on [this tutorial for `distributed-process-simplelocalnet`](http://hackage.haskell.org/package/distributed-process-simplelocalnet-0.2.0.9/docs/Control-Distributed-Process-Backend-SimpleLocalnet.html). It's a simple single-master/multiple slave system.
 
The pdf says *"Several nodes continuously send messages to other nodes in such way, ..."* but my code just has one sender, the *master*, and multiple slaves.
The slaves accumulate all the numbers they receive, according the the formula in the pdf, until `--wait-for` seconds have elapsed.  Then each slave prints the tuple.

## Running it

This probably won't work remotely as it should.
This relies on "node discovery based on UDP multicast" according to the above tutorial, and I don't know what your network allows.

See `run.bash` for a bash script to run this. It loads a number of slaves, then the master. The master delays itself for 0.5 seconds to ensure the slaves have enough time to get going and be visible on the network.

## command line

The `--send-for` and `--wait-for` options are compulsory. (They're not "options" then I guess!)

    simple-exe --send-for 5 --wait-for 1

`--with-seed` specifies the seed, defaulting to 1337
