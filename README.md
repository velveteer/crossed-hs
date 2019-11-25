# C R O S S E D 

## Generate a puzzle
```sh
cabal run crossed-exe 
```

## Options
```
cabal run crossed-exe -- --help
```

```
Usage: crossed-exe [-v|--visualize] [-b|--batchSize INT] [-g|--gridSize INT] 
                   [-s|--minStart INT] [-w|--words INT] [-l|--checkLimit INT]
  Generate a sparse crossword puzzle, i.e. given a list of dictionary words and
  their clues, find a populated grid that solves for the given constraints.

Available options:
  -v,--visualize           Print intermediate grids during generation.
  -b,--batchSize INT       Control the available word pool. Taken from a random
                           shuffle of ~283,000 words. Increases generation
                           time. (default: 1000)
  -g,--gridSize INT        Length and width of the grid. (default: 15)
  -s,--minStart INT        Minimum length for starting word. (default: 5)
  -w,--words INT           Number of words needed for a solution. Increases
                           generation time. (default: 36)
  -l,--checkLimit INT      Number of word placement attempts made before
                           returning a solution. (default: 200)
  -h,--help                Show this help text

```

