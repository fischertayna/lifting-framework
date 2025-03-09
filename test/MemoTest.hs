module MemoTest where

import Test.HUnit
import Language.MInterpreter.Interpreter
import Language.MInterpreter.Driver
import Base.Types (Valor(..))
import Helper

inputInt, inputBool, inputString, inputList, inputPair :: Valor
inputInt = ValorInt 10
inputBool = ValorBool True
inputString = ValorStr "test"
inputList = ValorList [ValorInt 10,  ValorInt 11, ValorInt 12, ValorInt 13]
inputListString = ValorList [
                      ValorStr "test",
                      ValorStr "_",
                      ValorStr "concatenated",
                      ValorStr "_",
                      ValorStr "from",
                      ValorStr "_",
                      ValorStr "list"
                    ]
inputPair = ValorPair (ValorInt 10, ValorInt 20)

testSimples :: Test
testSimples = TestCase $ do
    output <- processFile (executeProg ["conditional"] []) "src/Language/Examples/Simple.lng" inputInt
    let expectedOutput = (ValorInt 14) 
    assertEqual "Simple Conditional (3 + (if x then x+1 else x-1))" expectedOutput (fst output)

testFibonacci :: Test
testFibonacci = TestCase $ do
    output <- processFile (executeProg ["fib"] []) "src/Language/Examples/Fibonacci.lng" inputInt
    let expectedOutput = (ValorInt 55) 
    assertEqual "Fibonacci x" expectedOutput (fst output)

testFatorial :: Test
testFatorial = TestCase $ do
    output <- processFile (executeProg ["fat"] []) "src/Language/Examples/Fatorial.lng" inputInt
    let expectedOutput = (ValorInt 3628800) 
    assertEqual "Fatorial x" expectedOutput (fst output)

testSomaListaSimples :: Test
testSomaListaSimples = TestCase $ do
    output <- processFile (executeProg ["soma"] []) "src/Language/Examples/Lista.lng" inputInt
    let expectedOutput = (ValorInt 46) 
    assertEqual "Soma lista [x, x+1, x+2, x+3]" expectedOutput (fst output)

testSomaLista :: Test
testSomaLista = TestCase $ do
    output <- processFile (executeProg ["soma"] []) "src/Language/Examples/Lista-inputList.lng" inputList
    let expectedOutput = (ValorInt 46) 
    assertEqual "Soma lista" expectedOutput (fst output)

testSomaParSimples :: Test
testSomaParSimples = TestCase $ do
    output <- processFile (executeProg ["soma"] []) "src/Language/Examples/Pares.lng" inputInt
    let expectedOutput = (ValorInt 30) 
    assertEqual "Soma par (x, 20)" expectedOutput (fst output)

testSomaPar :: Test
testSomaPar = TestCase $ do
    output <- processFile (executeProg ["soma"] []) "src/Language/Examples/Pares-inputPair.lng" inputPair
    let expectedOutput = (ValorInt 30) 
    assertEqual "Soma par (x, y)" expectedOutput (fst output)

testBool :: Test
testBool = TestCase $ do
    output <- processFile (executeProg ["or"] []) "src/Language/Examples/Bool.lng" inputBool
    let expectedOutput = (ValorBool True) 
    assertEqual "Bool true" expectedOutput (fst output)

testConcatSimples :: Test
testConcatSimples = TestCase $ do
    output <- processFile (executeProg ["concat"] []) "src/Language/Examples/Concat.lng" inputString
    let expectedOutput = (ValorStr "test_concatenated") 
    assertEqual "concat x = x_concatenated" expectedOutput (fst output)

testConcatLista :: Test
testConcatLista = TestCase $ do
    output <- processFile (executeProg ["concat"] []) "src/Language/Examples/Concat-inputList.lng" inputListString
    let expectedOutput = (ValorStr "test_concatenated_from_list")
    assertEqual "Concat lista" expectedOutput (fst output)

testPolymorphicPairSameType :: Test
testPolymorphicPairSameType = TestCase $ do
    output <- processFile (executeProg ["invert"] []) "src/Language/Examples/Polymorphic-pair.lng" (
                    ValorPair (ValorStr "test", ValorStr "1"))
    let expectedOutput = (ValorPair (ValorStr "1", ValorStr "test"))
    assertEqual "Invert pair same type" expectedOutput (fst output)


testPolymorphicPair :: Test
testPolymorphicPair = TestCase $ do
    output <- processFile (executeProg ["invert"] []) "src/Language/Examples/Polymorphic-pair.lng" (
                    ValorPair (ValorStr "test", ValorBool True))
    let expectedOutput = (ValorPair (ValorBool True, ValorStr "test"))
    assertEqual "Invert pair" expectedOutput (fst output)


testPolymorphicList :: Test
testPolymorphicList = TestCase $ do
    output <- processFile (executeProg ["length"] []) "src/Language/Examples/Polymorphic-list.lng" (
                    ValorList [
                      ValorStr "test",
                      ValorInt 1,
                      ValorBool True,
                      ValorPair (ValorStr "Pair", ValorInt 2)
                    ])
    let expectedOutput = (ValorInt 4)
    assertEqual "length list" expectedOutput (fst output)


memoTestSuite :: Test
memoTestSuite = TestList [ TestLabel "Var testSimples" testSimples
                         , TestLabel "Var testFibonacci" testFibonacci
                         , TestLabel "Var testFatorial" testFatorial
                         , TestLabel "Var testSomaListaSimples" testSomaListaSimples
                         , TestLabel "Var testSomaLista" testSomaLista
                         , TestLabel "Var testSomaParSimples" testSomaParSimples
                         , TestLabel "Var testSomaPar" testSomaPar
                         , TestLabel "Var testBool" testBool
                         , TestLabel "Var testConcatSimples" testConcatSimples
                         , TestLabel "Var testConcatLista" testConcatLista
                         , TestLabel "Var testPolymorphicPairSameType" testPolymorphicPairSameType
                         , TestLabel "Var testPolymorphicPair" testPolymorphicPair
                         , TestLabel "Var testPolymorphicList" testPolymorphicList
                        ]