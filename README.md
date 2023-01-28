### Generate the Frontend from the .cf Grammar definition

bnfc --haskell --name-space=Language.Frontend --output=src ./src/Language/Language.cf

### Compiling and running one of the interpreters

mkdir -p build
ghc -isrc src/Language/Interpreter/Driver.hs -o build/Driver
./build/Driver < src/Language/Examples/Simple.lng
