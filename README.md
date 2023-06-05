# tetris-haskell
Tetris implementation using haskell

## Requirements

- ghc version `9.2.7`
- cabal version `3.6.2.0`

For the previous ones we recommend to use [GHCup](https://www.haskell.org/ghcup/),
the main installer for the general purpose language Haskell.

To install [GHCup](https://www.haskell.org/ghcup/) 

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
- OpenGL and the corresponding development libraries (libgl1-mesa-dev, libglu1-mesa-dev, freeglut3-dev)

And to install the previous ones

```bash
sudo apt-get install libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev
```

## Execution

Navigate to the `tetris-haskell` directory

To ensure you have the latest package lists and dependencies, update 
Cabal by running the following command:

```bash
cabal v2-update
```

To compile the Tetris Haskell project, use the following command:

```bash
cabal v2-build
```

To compile and run the Tetris Haskell program, use the following command:

```bash
cabal v2-run
```
