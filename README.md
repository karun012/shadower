#shadower ![alt text](https://travis-ci.org/karun012/shadower.svg?branch=master)

Shadower watches a folder that you specify, and runs doctests in any Haskell sources in the folder that are modified


##Setup

Shadower is published on [hackage](http://hackage.haskell.org/package/shadower-0.1.0.2)

    cabal update
    cabal install shadower

##Usage  

###To monitor a specific path
    shadower <path to monitor> 

###To monitor the current folder
    shadower
