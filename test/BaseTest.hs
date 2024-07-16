module BaseTest where

import Language.Interpreter.Interpreter
import Language.Interpreter.Driver

import Language.Frontend.ErrM
import Helper

import Test.HUnit

testSimples :: Test
testSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Simple.lng" (ValorInt 10)
    let expectedOutput = (ValorInt 14)
    assertEqual "Simple Conditional (3 + (if x then x+1 else x-1))" expectedOutput output

testFibonacci :: Test
testFibonacci = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Fibonacci.lng" (ValorInt 10)
    let expectedOutput = (ValorInt 89)
    assertEqual "Fibonacci x" expectedOutput output

testFatorial :: Test
testFatorial = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Fatorial.lng" (ValorInt 10)
    let expectedOutput = (ValorInt 3628800)
    assertEqual "Fatorial x" expectedOutput output

testSomaListaSimples :: Test
testSomaListaSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Lista.lng" (ValorInt 10)
    let expectedOutput = (ValorInt 46)
    assertEqual "Soma lista [x, x+1, x+2, x+3]" expectedOutput output

testSomaLista :: Test
testSomaLista = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Lista-inputList.lng" (
                    ValorList [
                      ValorInt 10,
                      ValorInt 11,
                      ValorInt 12,
                      ValorInt 13
                    ])
    let expectedOutput = (ValorInt 46)
    assertEqual "Soma lista" expectedOutput output

testSomaParSimples :: Test
testSomaParSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Pares.lng" (ValorInt 10)
    let expectedOutput = (ValorInt 30)
    assertEqual "Soma par (x, 20)" expectedOutput output

testSomaPar :: Test
testSomaPar = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Pares-inputPair.lng" (
                    ValorPair (ValorInt 10, ValorInt 20))
    let expectedOutput = (ValorInt 30)
    assertEqual "Soma par (x, y)" expectedOutput output

testBoolTrue :: Test
testBoolTrue = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Bool.lng" (ValorBool True)
    let expectedOutput = (ValorBool True)
    assertEqual "Bool true" expectedOutput output

testBoolFalse :: Test
testBoolFalse = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Bool.lng" (ValorBool False)
    let expectedOutput = (ValorBool False)
    assertEqual "Bool false" expectedOutput output

testConcatSimples :: Test
testConcatSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Concat.lng" (ValorStr "test")
    let expectedOutput = (ValorStr "test_concatenated")
    assertEqual "concat x" expectedOutput output

testConcatLista :: Test
testConcatLista = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Concat-inputList.lng" (
                    ValorList [
                      ValorStr "test",
                      ValorStr "_",
                      ValorStr "concatenated",
                      ValorStr "_",
                      ValorStr "from",
                      ValorStr "_",
                      ValorStr "list"
                    ])
    let expectedOutput = (ValorStr "test_concatenated_from_list")
    assertEqual "Concat lista" expectedOutput output


baseTestSuite :: Test
baseTestSuite = TestList [ TestLabel "Base testSimples" testSimples
                         , TestLabel "Base testFibonacci" testFibonacci
                         , TestLabel "Base testFatorial" testFatorial
                         , TestLabel "Base testSomaListaSimples" testSomaListaSimples
                         , TestLabel "Base testSomaLista" testSomaLista
                         , TestLabel "Base testSomaParSimples" testSomaParSimples
                         , TestLabel "Base testSomaPar" testSomaPar
                         , TestLabel "Base testBoolTrue" testBoolTrue
                         , TestLabel "Base testBoolFalse" testBoolFalse
                         , TestLabel "Base testConcatSimples" testConcatSimples
                         , TestLabel "Base testConcatLista" testConcatLista
                        ]