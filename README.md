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

## Text processing

TODO: Learn parsec

Maybe use packages like `pipes` or `conduit`

## Sources

- <https://www.reddit.com/r/haskell/comments/181hwly/how_to_install_haskell_packageslibraries_globally/>
- <https://wiki.haskell.org/Regular_expressions>
- <https://wjwh.eu/posts/2022-11-30-haskell-aoc-tricks.html>
- <https://blog.jle.im/entry/io-monad-considered-harmful.html>
- <https://github.com/mstksg/advent-of-code/wiki/Reflections-2024>
