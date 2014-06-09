#shadower [![build status](https://travis-ci.org/karun012/shadower.png)](https://travis-ci.org/karun012/shadower)

Shadower watches a folder that you specify, and runs doctests in any Haskell sources in the folder that are modified


##Setup

Shadower is published on [hackage](https://hackage.haskell.org/package/shadower)

    cabal update
    cabal install shadower

##Usage  

###To monitor a specific path
    shadower <path to monitor> 

###To monitor the current folder
    shadower
