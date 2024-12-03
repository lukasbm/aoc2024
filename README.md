# aoc2024

Advent of Code 2024 in Haskell

## Tips

```haskell
main = do
  line <- getLine
  putStrLn line
  main 
```
is the same as:
```haskell
main = getLine >>= putStrLn >> main
```

## Sources

- <https://www.reddit.com/r/haskell/comments/181hwly/how_to_install_haskell_packageslibraries_globally/>
- <https://wiki.haskell.org/Regular_expressions>
