module BaseTest where

import Language.Interpreter.Interpreter
import Language.Interpreter.Driver

import Language.Frontend.ErrM
import Helper

import Test.HUnit
import System.Timeout (timeout)
import Control.Exception (evaluate)
import Base.Types (Valor(..))

timeoutP :: Int -> IO a -> IO (Maybe a)
timeoutP t fun = timeout t $ fun

testSimples :: Test
testSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Simple.lng" (ValorInt 10)
    let expectedOutput = (ValorInt 14)
    assertEqual "Simple Conditional (3 + (if x then x+1 else x-1))" expectedOutput output

testFibonacci :: Test
testFibonacci = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Fibonacci.lng" (ValorInt 10)
    let expectedOutput = (ValorInt 55)
    assertEqual "Fibonacci 10" expectedOutput output

-- testHighFibonacci :: Test
-- testHighFibonacci = TestCase $ do
--     result <- timeoutP (1 * 100000) $ (processFile executeProg "src/Language/Analysis/Fibonacci.lng" (ValorInt 100))
--     let expectedOutput = (ValorInt 354224848179261915075)
--     case result of
--         Nothing -> assertFailure "Fibonacci 100 timed out!"
--         Just output  -> assertEqual "Fibonacci 100" expectedOutput output

testFatorial :: Test
testFatorial = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Fatorial.lng" (ValorInt 10)
    let expectedOutput = (ValorInt 3628800)
    assertEqual "Fatorial x" expectedOutput output

testSomaListaSimples :: Test
testSomaListaSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Lista.lng" (ValorInt 10)
    let expectedOutput = (ValorInt 46)
    assertEqual "Soma lista [x, x+1, x+2, x+3]" expectedOutput output

testSomaLista :: Test
testSomaLista = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Lista-inputList.lng" (
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
    output <- processFile executeProg "src/Language/Analysis/Pares.lng" (ValorInt 10)
    let expectedOutput = (ValorInt 30)
    assertEqual "Soma par (x, 20)" expectedOutput output

testSomaPar :: Test
testSomaPar = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Pares-inputPair.lng" (
                    ValorPair (ValorInt 10, ValorInt 20))
    let expectedOutput = (ValorInt 30)
    assertEqual "Soma par (x, y)" expectedOutput output

testBoolTrue :: Test
testBoolTrue = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Bool.lng" (ValorBool True)
    let expectedOutput = (ValorBool True)
    assertEqual "Bool true" expectedOutput output

testBoolFalse :: Test
testBoolFalse = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Bool.lng" (ValorBool False)
    let expectedOutput = (ValorBool False)
    assertEqual "Bool false" expectedOutput output

testConcatSimples :: Test
testConcatSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Concat.lng" (ValorStr "test")
    let expectedOutput = (ValorStr "test_concatenated")
    assertEqual "concat x" expectedOutput output

testConcatLista :: Test
testConcatLista = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Concat-inputList.lng" (
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

testConcatTwoLists :: Test
testConcatTwoLists = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Concat-list.lng" (
                    ValorPair (
                        ValorList [
                            ValorStr "test",
                            ValorStr "_",
                            ValorStr "concatenated"
                        ], 
                        ValorList [
                            ValorStr "_",
                            ValorStr "from",
                            ValorStr "_",
                            ValorStr "list"
                        ]
                    ))
    let expectedOutput = (ValorList [
                            ValorStr "test",
                            ValorStr "_",
                            ValorStr "concatenated",
                            ValorStr "_",
                            ValorStr "from",
                            ValorStr "_",
                            ValorStr "list"
                        ])
    assertEqual "Concat lista" expectedOutput output

testPolymorphicPairSameType :: Test
testPolymorphicPairSameType = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Polymorphic-pair.lng" (
                    ValorPair (ValorStr "test", ValorStr "1"))
    let expectedOutput = (ValorPair (ValorStr "1", ValorStr "test"))
    assertEqual "Invert pair same type" expectedOutput output


testPolymorphicPair :: Test
testPolymorphicPair = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Polymorphic-pair.lng" (
                    ValorPair (ValorStr "test", ValorBool True))
    let expectedOutput = (ValorPair (ValorBool True, ValorStr "test"))
    assertEqual "Invert pair" expectedOutput output


testPolymorphicList :: Test
testPolymorphicList = TestCase $ do
    output <- processFile executeProg "src/Language/Analysis/Polymorphic-list.lng" (
                    ValorList [
                      ValorStr "test",
                      ValorInt 1,
                      ValorBool True,
                      ValorPair (ValorStr "Pair", ValorInt 2)
                    ])
    let expectedOutput = (ValorInt 4)
    assertEqual "length list" expectedOutput output


baseTestSuite :: Test
baseTestSuite = TestList [ TestLabel "Base testSimples" testSimples
                         , TestLabel "Base testFibonacci" testFibonacci
                        --  , TestLabel "Base testHighFibonacci" testHighFibonacci
                         , TestLabel "Base testFatorial" testFatorial
                         , TestLabel "Base testSomaListaSimples" testSomaListaSimples
                         , TestLabel "Base testSomaLista" testSomaLista
                         , TestLabel "Base testSomaParSimples" testSomaParSimples
                         , TestLabel "Base testSomaPar" testSomaPar
                         , TestLabel "Base testBoolTrue" testBoolTrue
                         , TestLabel "Base testBoolFalse" testBoolFalse
                         , TestLabel "Base testConcatSimples" testConcatSimples
                         , TestLabel "Base testConcatLista" testConcatLista
                         , TestLabel "Base testConcatTwoLists" testConcatTwoLists
                         , TestLabel "Base testPolymorphicPairSameType" testPolymorphicPairSameType
                         , TestLabel "Base testPolymorphicPair" testPolymorphicPair
                         , TestLabel "Base testPolymorphicList" testPolymorphicList
                        ]