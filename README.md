### Installing and Compiling
Check INSTALL.md

### Generate the Frontend from the .cf Grammar definition

```
bnfc --haskell --name-space=Language.Frontend --output=src ./src/Language/Language.cf
```

### Running one of the interpreters

```
cabal run < src/Language/Examples/Simple.lng
```

<!-- mkdir -p build
ghc -isrc src/Language/Interpreter/Driver.hs -o build/Driver
./build/Driver < src/Language/Examples/Simple.lng -->

### Running Tests

```
cabal test --test-show-details=streaming
```

You can change what tests will be run in the file Spec.hs.