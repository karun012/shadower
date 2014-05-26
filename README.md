#shadower

Shadower watches a folder that you specify, and runs doctests on any Haskell sources in the folder that are modified


##Setup

Shadower is not published on [hackage](http://hackage.haskell.org) yet

Download the [latest release](https://github.com/karun012/shadower/releases/latest). Extract it

    cabal configure
    cabal install

##Usage  

###To monitor a specific path
    shadower <path to monitor> 

###To monitor the current folder
    shadower
