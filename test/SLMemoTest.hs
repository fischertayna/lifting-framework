module SLMemoTest where

import Test.HUnit
import Language.MInterpreter.Interpreter
import Language.MInterpreter.SLDriver
import Variability.VarTypes (Prop, Var (Var), apply, mkBDDVar, notBDD, ttPC, (/\), (\/), (|||))
import Helper

propA :: Prop
propA = mkBDDVar "A"

propB :: Prop
propB = mkBDDVar "B"

atbt :: Prop
atbt = propA /\ propB

atbf :: Prop
atbf = propA /\ notBDD propB

afbt :: Prop
afbt = notBDD propA /\ propB

afbf :: Prop
afbf = notBDD propA /\ notBDD propB

inputInt, inputBool, inputString, inputList, inputPair :: Var Valor
inputInt = (Var [(ValorInt 2, atbt), (ValorInt 3, afbt), (ValorInt 4, atbf), (ValorInt 5, afbf)])
inputBool = (Var [(ValorBool True, atbt), (ValorBool False, afbt), (ValorBool False, atbf), (ValorBool False, afbf)])
inputString = (Var [(ValorStr "abc", atbt), (ValorStr "def", afbt),  (ValorStr "ghi", atbf), (ValorStr "jkl", afbf)])
inputListString = (Var [(ValorList [ValorStr "abc",  ValorStr "def", ValorStr "ghi"], atbt),
                  (ValorList [ValorStr "123",  ValorStr "456", ValorStr "789"], afbt),
                  (ValorList [ValorStr "hello",  ValorStr "_", ValorStr "world"], atbf),
                  (ValorList [ValorStr "this_",  ValorStr "is_", ValorStr "a_", ValorStr "test"], afbf)])

inputList = (Var [(ValorList [ValorInt 8,  ValorInt 2, ValorInt 3], atbt),
                  (ValorList [ValorInt 5,  ValorInt 1, ValorInt 2], afbt),
                  (ValorList [ValorInt 0,  ValorInt 4, ValorInt 5], atbf),
                  (ValorList [ValorInt 1,  ValorInt 6, ValorInt 3], afbf)])
inputPair = (Var [(ValorPair (ValorInt 8, ValorInt 2), atbt), 
                  (ValorPair (ValorInt 5, ValorInt 1), afbt), 
                  (ValorPair (ValorInt 0, ValorInt 4), atbf), 
                  (ValorPair (ValorInt 1, ValorInt 6), afbf)])

testSimples :: Test
testSimples = TestCase $ do
    output <- processFile (executeProg "conditional") "src/Language/Examples/Simple.lng" inputInt
    let expectedValor = (Var [(ValorInt 6, atbt), (ValorInt 7, afbt), (ValorInt 8, atbf), (ValorInt 9, afbf)])
    let expectedMem = [ ([ValorInt 5], ValorInt 6),
                        ([ValorInt 4], ValorInt 5),
                        ([ValorInt 3], ValorInt 4),
                        ([ValorInt 2], ValorInt 3)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Simple Conditional (3 + (if x then x+1 else x-1))" expectedOutput (output)

testFibonacci :: Test
testFibonacci = TestCase $ do
    output <- processFile (executeProg "fib") "src/Language/Examples/Fibonacci.lng" inputInt
    let expectedValor = (Var [(ValorInt 2, atbt), (ValorInt 3, afbt), (ValorInt 5, atbf), (ValorInt 8, afbf)])
    let expectedMem = [([ValorInt 5], ValorInt 8),
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
    let expectedValor = (Var [(ValorInt 2, atbt), (ValorInt 6, afbt), (ValorInt 24, atbf), (ValorInt 120, afbf)])
    let expectedMem = [([ValorInt 5], ValorInt 120),
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
    let expectedValor = (Var [(ValorInt 14, atbt), (ValorInt 18, afbt), (ValorInt 22, atbf), (ValorInt 26, afbf)])
    let expectedMem = [([ValorList [ValorInt 5, ValorInt 6, ValorInt 7, ValorInt 8]], ValorInt 26),
                          ([ValorList [ValorInt 6, ValorInt 7, ValorInt 8]], ValorInt 21),
                          ([ValorList [ValorInt 7, ValorInt 8]], ValorInt 15),
                          ([ValorList [ValorInt 8]], ValorInt 8),
                          ([ValorList [ValorInt 4, ValorInt 5, ValorInt 6, ValorInt 7]], ValorInt 22),
                          ([ValorList [ValorInt 5, ValorInt 6, ValorInt 7]], ValorInt 18),
                          ([ValorList [ValorInt 6, ValorInt 7]], ValorInt 13),
                          ([ValorList [ValorInt 7]], ValorInt 7),
                          ([ValorList [ValorInt 3, ValorInt 4, ValorInt 5, ValorInt 6]], ValorInt 18),
                          ([ValorList [ValorInt 4, ValorInt 5, ValorInt 6]], ValorInt 15),
                          ([ValorList [ValorInt 5, ValorInt 6]], ValorInt 11),
                          ([ValorList [ValorInt 6]], ValorInt 6),
                          ([ValorList [ValorInt 2, ValorInt 3, ValorInt 4, ValorInt 5]], ValorInt 14),
                          ([ValorList [ValorInt 3, ValorInt 4, ValorInt 5]], ValorInt 12),
                          ([ValorList [ValorInt 4, ValorInt 5]], ValorInt 9),
                          ([ValorList [ValorInt 5]], ValorInt 5),
                          ([ValorList []], ValorInt 0)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Soma lista [x, x+1, x+2, x+3]" expectedOutput output

testSomaLista :: Test
testSomaLista = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Lista-inputList.lng" inputList
    let expectedValor = (Var [(ValorInt 13, atbt), (ValorInt 8, afbt), (ValorInt 9, atbf), (ValorInt 10, afbf)])
    let expectedMem = [([ValorList [ValorInt 1, ValorInt 6, ValorInt 3]], ValorInt 10),
                          ([ValorList [ValorInt 6, ValorInt 3]], ValorInt 9),
                          ([ValorList [ValorInt 0, ValorInt 4, ValorInt 5]], ValorInt 9),
                          ([ValorList [ValorInt 4, ValorInt 5]], ValorInt 9),
                          ([ValorList [ValorInt 5]], ValorInt 5),
                          ([ValorList [ValorInt 5, ValorInt 1, ValorInt 2]], ValorInt 8),
                          ([ValorList [ValorInt 1, ValorInt 2]], ValorInt 3),
                          ([ValorList [ValorInt 2]], ValorInt 2),
                          ([ValorList [ValorInt 8, ValorInt 2, ValorInt 3]], ValorInt 13),
                          ([ValorList [ValorInt 2, ValorInt 3]], ValorInt 5),
                          ([ValorList [ValorInt 3]], ValorInt 3),
                          ([ValorList []], ValorInt 0)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Soma lista" expectedOutput output

testSomaParSimples :: Test
testSomaParSimples = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Pares.lng" inputInt
    let expectedValor = (Var [(ValorInt 22, atbt), (ValorInt 23, afbt), (ValorInt 24, atbf), (ValorInt 25, afbf)])
    let expectedMem = [([ValorPair (ValorInt 5, ValorInt 20)], ValorInt 25),
                          ([ValorPair (ValorInt 4, ValorInt 20)], ValorInt 24),
                          ([ValorPair (ValorInt 3, ValorInt 20)], ValorInt 23),
                          ([ValorPair (ValorInt 2, ValorInt 20)], ValorInt 22)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Soma par (x, 20)" expectedOutput output

testSomaPar :: Test
testSomaPar = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Pares-inputPair.lng" inputPair
    let expectedValor = (Var [(ValorInt 10, atbt), (ValorInt 6, afbt), (ValorInt 4, atbf), (ValorInt 7, afbf)])
    let expectedMem = [([ValorPair (ValorInt 1, ValorInt 6)], ValorInt 7),
                          ([ValorPair (ValorInt 0, ValorInt 4)], ValorInt 4),
                          ([ValorPair (ValorInt 5, ValorInt 1)], ValorInt 6),
                          ([ValorPair (ValorInt 8, ValorInt 2)], ValorInt 10)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Soma par (x, y)" expectedOutput output

testBool :: Test
testBool = TestCase $ do
    output <- processFile (executeProg "or") "src/Language/Examples/Bool.lng" inputBool
    let expectedValor = (Var [(ValorBool True, atbt), (ValorBool False, afbt), (ValorBool False, atbf), (ValorBool False, afbf)])
    let expectedMem = [([ValorBool True, ValorBool False], ValorBool True),
                          ([ValorBool False, ValorBool True], ValorBool True)]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Bool true" expectedOutput output

testConcatSimples :: Test
testConcatSimples = TestCase $ do
    output <- processFile (executeProg "concat") "src/Language/Examples/Concat.lng" inputString
    let expectedValor = (Var [(ValorStr "abc_concatenated", atbt), 
                              (ValorStr "def_concatenated", afbt), 
                              (ValorStr "ghi_concatenated", atbf), 
                              (ValorStr "jkl_concatenated", afbf)])
    let expectedMem = [([ValorStr "jkl", ValorStr "_concatenated"], ValorStr "jkl_concatenated"),
                          ([ValorStr "ghi", ValorStr "_concatenated"], ValorStr "ghi_concatenated"),
                          ([ValorStr "def", ValorStr "_concatenated"], ValorStr "def_concatenated"),
                          ([ValorStr "abc", ValorStr "_concatenated"], ValorStr "abc_concatenated")]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "concat x = x_concatenated" expectedOutput output

testConcatLista :: Test
testConcatLista = TestCase $ do
    output <- processFile (executeProg "concat") "src/Language/Examples/Concat-inputList.lng" inputListString
    let expectedValor = (Var [(ValorStr "abcdefghi", atbt), 
                              (ValorStr "123456789", afbt), 
                              (ValorStr "hello_world", atbf), 
                              (ValorStr "this_is_a_test", afbf)])
    let expectedMem = [ ([ValorList [ValorStr "this_", ValorStr "is_", ValorStr "a_", ValorStr "test"]], ValorStr "this_is_a_test")
                         , ([ValorList [ValorStr "is_", ValorStr "a_", ValorStr "test"]], ValorStr "is_a_test")
                         , ([ValorList [ValorStr "a_", ValorStr "test"]], ValorStr "a_test")
                         , ([ValorList [ValorStr "test"]], ValorStr "test")
                         , ([ValorList [ValorStr "hello", ValorStr "_", ValorStr "world"]], ValorStr "hello_world")
                         , ([ValorList [ValorStr "_", ValorStr "world"]], ValorStr "_world")
                         , ([ValorList [ValorStr "world"]], ValorStr "world")
                         , ([ValorList [ValorStr "123", ValorStr "456", ValorStr "789"]], ValorStr "123456789")
                         , ([ValorList [ValorStr "456", ValorStr "789"]], ValorStr "456789")
                         , ([ValorList [ValorStr "789"]], ValorStr "789")
                         , ([ValorList [ValorStr "abc", ValorStr "def", ValorStr "ghi"]], ValorStr "abcdefghi")
                         , ([ValorList [ValorStr "def", ValorStr "ghi"]], ValorStr "defghi")
                         , ([ValorList [ValorStr "ghi"]], ValorStr "ghi")
                         , ([ValorList []], ValorStr "")
                         ]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Concat lista" expectedOutput output

slMemoTestSuite :: Test
slMemoTestSuite = TestList [ TestLabel "Var testSimples" testSimples
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