Write Yourself A Scheme In 48 Hours
===================================

These are my solution to the Haskell [wiki tutorial](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

You can try it out easily by:

    cd ch10
    ghci listing_1.hs

... and then in ghci do:

    > runRepl
    >>> (load "stdlib.scm")
    >>> (min '(10 9 8 7 6 5 4))
    4