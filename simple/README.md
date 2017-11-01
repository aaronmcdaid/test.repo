# simple

Clarify what I did, as it is a bit less than was asked for.

The pdf says *"Several nodes continuously send messages to other nodes in such way, ..."*
but my code just has one sender, the *master*, and multiple slaves.
The slaves accumulate all the numbers they receive, according the the formula in the pdf,
until `--wait-for` seconds have elapsed.
Then each slave prints the tuple.

## command line

The `--send-for` and `--wait-for` options are compulsory. (They're not "options" then I guess!)

    simple-exe --send-for 5 --wait-for 1

`--with-seed` specifies the seed, defaulting to 1337

## `nodes.txt`

This program loads `nodes.txt` in the working directory for the list of nodes. IP address and port number, separated by colon. For example:

    127.0.0.1:10301
    127.1.0.5:10501
    127.1.0.7:10701

## network failure

...

