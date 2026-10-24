# ram-machine-hs

Interpreter for the RAM Machine in Haskell.

## Syntax

Work in progress. Good luck.

## Installation

Make sure you have the Haskell toolchain with GHC 9.10.x and Cabal 3.x. If you don't, you can install it with [GHCup](https://www.haskell.org/ghcup/).

To set up, clone and build the repository:
```
git clone https://github.com/s-feles/ram-machine-hs
cd rammachine
cabal build
```

You can now run the executable with `cabal run rammachine -- [args]`. However, you can also install it with
```
cabal install exe:rammachine --overwrite-policy:always
```
so that you can just run `rammachine [args]` directly.

## Usage
Replace the `[args]` in either command with the file path relative to your working directory followed by the contents of the input tape. For example, to run the example `log.ram` program from the project root, run
```
rammachine app/log.ram 5 4 3 2 1 -1 2
```
which corresponds to running the program `log.ram` with the input `5 4 3 2 1 -1 2`. Alternatively, from the project root you can run
```
cabal run rammachine -- app/log.ram 5 4 3 2 1 -1 2
```
for the same effect.

The output should show the state of the output tape upon the machine halting.