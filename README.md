# First steps in haskell
The following should act as a walkthrough from installing haskell, over loading your first module, to running your first small program.

## Installation
apt install haskell-platform

## startup
to startup up a interactive haskell interpreter:
``` sh
ghci
``` 
afterwards to load, reload modules:
``` sh
:l FirstSteps.hs
:r
```
to find out about a method / type:
``` sh
:t map 
```

## usage
once the module is loaded you may call methods like this:
``` haskell
areStringEq "hello" ('h':'e':'l':'l':'o':[])
```