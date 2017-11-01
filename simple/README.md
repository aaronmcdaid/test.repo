# simple

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
