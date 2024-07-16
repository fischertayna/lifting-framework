module MemoTest where

import Test.HUnit
import Language.MInterpreter.Interpreter
import Language.MInterpreter.Driver
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
    output <- processFile (executeProg "conditional") "src/Language/Examples/Simple.lng" inputInt
    let expectedValor = (ValorInt 14) 
    let expectedMem = [ ([ValorInt 10], ValorInt 11) ]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Simple Conditional (3 + (if x then x+1 else x-1))" expectedOutput output

testFibonacci :: Test
testFibonacci = TestCase $ do
    output <- processFile (executeProg "fib") "src/Language/Examples/Fibonacci.lng" inputInt
    let expectedValor = (ValorInt 89) 
    let expectedMem = [([ValorInt 10], ValorInt 89),
                       ([ValorInt 9], ValorInt 55),
                       ([ValorInt 8], ValorInt 34),
                       ([ValorInt 7], ValorInt 21),
                       ([ValorInt 6], ValorInt 13),
                       ([ValorInt 5], ValorInt 8),
                       ([ValorInt 4], ValorInt 5),
                       ([ValorInt 3], ValorInt 3),
                       ([ValorInt 2], ValorInt 2),
                       ([ValorInt 0], ValorInt 1),
                       ([ValorInt 1], ValorInt 1)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Fibonacci x" expectedOutput output

testFatorial :: Test
testFatorial = TestCase $ do
    output <- processFile (executeProg "fat") "src/Language/Examples/Fatorial.lng" inputInt
    let expectedValor = (ValorInt 3628800) 
    let expectedMem = [([ValorInt 10], ValorInt 3628800),
                       ([ValorInt 9], ValorInt 362880),
                       ([ValorInt 8], ValorInt 40320),
                       ([ValorInt 7], ValorInt 5040),
                       ([ValorInt 6], ValorInt 720),
                       ([ValorInt 5], ValorInt 120),
                       ([ValorInt 4], ValorInt 24),
                       ([ValorInt 3], ValorInt 6),
                       ([ValorInt 2], ValorInt 2),
                       ([ValorInt 1], ValorInt 1),
                       ([ValorInt 0], ValorInt 1)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Fatorial x" expectedOutput output

testSomaListaSimples :: Test
testSomaListaSimples = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Lista.lng" inputInt
    let expectedValor = (ValorInt 46) 
    let expectedMem = [([ValorList [ValorInt 10, ValorInt 11, ValorInt 12, ValorInt 13]], ValorInt 46),
                       ([ValorList [ValorInt 11, ValorInt 12, ValorInt 13]], ValorInt 36),
                       ([ValorList [ValorInt 12, ValorInt 13]], ValorInt 25),
                       ([ValorList [ValorInt 13]], ValorInt 13),
                       ([ValorList []], ValorInt 0)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Soma lista [x, x+1, x+2, x+3]" expectedOutput output

testSomaLista :: Test
testSomaLista = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Lista-inputList.lng" inputList
    let expectedValor = (ValorInt 46) 
    let expectedMem = [([ValorList [ValorInt 10, ValorInt 11, ValorInt 12, ValorInt 13]], ValorInt 46),
                       ([ValorList [ValorInt 11, ValorInt 12, ValorInt 13]], ValorInt 36),
                       ([ValorList [ValorInt 12, ValorInt 13]], ValorInt 25),
                       ([ValorList [ValorInt 13]], ValorInt 13),
                       ([ValorList []], ValorInt 0)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Soma lista" expectedOutput output

testSomaParSimples :: Test
testSomaParSimples = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Pares.lng" inputInt
    let expectedValor = (ValorInt 30) 
    let expectedMem = [([ValorPair (ValorInt 10, ValorInt 20)], ValorInt 30)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Soma par (x, 20)" expectedOutput output

testSomaPar :: Test
testSomaPar = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Pares-inputPair.lng" inputPair
    let expectedValor = (ValorInt 30) 
    let expectedMem = [([ValorPair (ValorInt 10, ValorInt 20)], ValorInt 30)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Soma par (x, y)" expectedOutput output

testBool :: Test
testBool = TestCase $ do
    output <- processFile (executeProg "or") "src/Language/Examples/Bool.lng" inputBool
    let expectedValor = (ValorBool True) 
    let expectedMem = [([ValorBool False, ValorBool True], ValorBool True)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Bool true" expectedOutput output

testConcatSimples :: Test
testConcatSimples = TestCase $ do
    output <- processFile (executeProg "concat") "src/Language/Examples/Concat.lng" inputString
    let expectedValor = (ValorStr "test_concatenated") 
    let expectedMem = [([ValorStr "test", ValorStr "_concatenated"], ValorStr "test_concatenated")]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "concat x = x_concatenated" expectedOutput output

testConcatLista :: Test
testConcatLista = TestCase $ do
    output <- processFile (executeProg "concat") "src/Language/Examples/Concat-inputList.lng" inputListString
    let expectedValor = (ValorStr "test_concatenated_from_list") 
    let expectedMem = [([ValorList [
                      ValorStr "test",
                      ValorStr "_",
                      ValorStr "concatenated",
                      ValorStr "_",
                      ValorStr "from",
                      ValorStr "_",
                      ValorStr "list"
                    ]], ValorStr "test_concatenated_from_list"),
                    ([ValorList [
                      ValorStr "_",
                      ValorStr "concatenated",
                      ValorStr "_",
                      ValorStr "from",
                      ValorStr "_",
                      ValorStr "list"
                    ]], ValorStr "_concatenated_from_list"),
                    ([ValorList [
                      ValorStr "concatenated",
                      ValorStr "_",
                      ValorStr "from",
                      ValorStr "_",
                      ValorStr "list"
                    ]], ValorStr "concatenated_from_list"),
                    ([ValorList [
                      ValorStr "_",
                      ValorStr "from",
                      ValorStr "_",
                      ValorStr "list"
                    ]], ValorStr "_from_list"),
                    ([ValorList [
                      ValorStr "from",
                      ValorStr "_",
                      ValorStr "list"
                    ]], ValorStr "from_list"),
                    ([ValorList [
                      ValorStr "_",
                      ValorStr "list"
                    ]], ValorStr "_list"),
                    ([ValorList [
                      ValorStr "list"
                    ]], ValorStr "list"),
                    ([ValorList []], ValorStr "")]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Concat lista" expectedOutput output

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
                        ]