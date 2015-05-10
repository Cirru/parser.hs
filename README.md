
Cirru Parser in Haskell
----

Reimplement Cirru Parser in Haskell.

### Usage

Docs https://hackage.haskell.org/package/cirru-parser

```
cabal install cirru-parser
```

```haskell
import CirruParser

main = do
  let
    tree = parse "code" "filename"
    simpleTree = pare "code" "filename"
  print $ show tree
  print $ show simpleTree
```

### Develope

Test:

```bash
cd src/
runghc main.hs
```

### License

MIT
