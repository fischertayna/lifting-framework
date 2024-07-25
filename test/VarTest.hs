module VarTest where

import Language.VInterpreter.Interpreter
import Variability.VarTypes (PresenceCondition, VarValor(..), Var (Var), Val, ttPC, ffPC, Prop, mkBDDVar, (/\), notBDD, (|||))
import Language.VInterpreter.Driver
import Helper

import Test.HUnit

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

inputInt, inputBool, inputString, inputList, inputPair :: VarValor
inputInt = VarInteger (Var [(2, atbt), (3, afbt),  (4, atbf), (5, afbf)])
inputBool = VarBool (Var [(True, atbt), (False, afbt),  (False, atbf), (False, afbf)])
inputString = VarString (Var [("abc", atbt), ( "def", afbt),  ("ghi", atbf), ("jkl", afbf)])
inputListString = VarList [
                      VarString (Var [("abc", atbt), ( "123", afbt),  ("hello", atbf), ("this_", afbf)]),
                      VarString (Var [("def", atbt), ( "456", afbt),  ("_", atbf), ("is_", afbf)]),
                      VarString (Var [("ghi", atbt), ( "789", afbt),  ("world", atbf), ("a_test", afbf)])
                    ]
inputList = VarList [
                      VarInteger (Var [(8, atbt), ( 5, afbt), (0, atbf), (1, afbf)]),
                      VarInteger (Var [(2, atbt), ( 1, afbt), (4, atbf), (6, afbf)]),
                      VarInteger (Var [(3, atbt), ( 2, afbt), (5, atbf), (3, afbf)])
                    ]
inputPolymorphicList = VarList [
                      VarInteger (Var [(8, atbt), ( 5, afbt), (0, atbf), (1, afbf)]),
                      VarBool (Var [(True, atbt), (False, afbt),  (False, atbf), (False, afbf)]),
                      VarString (Var [("abc", atbt), ( "def", afbt),  ("ghi", atbf), ("jkl", afbf)])
                    ]
inputPolymorphicListIncomplete = VarList [
                      VarInteger (Var [(8, atbt), ( 5, afbt), (0, atbf), (1, afbf)]),
                      VarBool (Var [(False, afbt),  (False, atbf)]),
                      VarString (Var [( "def", afbt),  ("ghi", atbf), ("jkl", afbf)])
                    ]
inputPair = VarPair (VarInteger (Var [(8, atbt), ( 5, afbt),  (0, atbf), (1, afbf)]), VarInteger (Var [(2, atbt), ( 1, afbt),  (4, atbf), (6, afbf)]))
inputPolymorphicPair = VarPair (VarInteger (Var [(8, atbt), ( 5, afbt),  (0, atbf), (1, afbf)]), VarBool (Var [(True, atbt), (False, afbt),  (False, atbf), (False, afbf)]))

testSimples :: Test
testSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Simple.lng" inputInt
    let expectedOutput = (VarInteger (Var [(6, atbt), (7, afbt), (8, atbf), (9, afbf)]))
    assertEqual "Simple Conditional (3 + (if x then x+1 else x-1))" expectedOutput output

testFibonacci :: Test
testFibonacci = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Fibonacci.lng" inputInt
    let expectedOutput = (VarInteger (Var [(5, afbf), (3, atbf), (2, afbt), (1, atbt)]))
    assertEqual "Fibonacci x" expectedOutput output

testFatorial :: Test
testFatorial = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Fatorial.lng" inputInt
    let expectedOutput = (VarInteger (Var [(2, atbt), (6, afbt), (24, atbf), (120, afbf)]))
    assertEqual "Fatorial x" expectedOutput output

testSomaListaSimples :: Test
testSomaListaSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Lista.lng" inputInt
    let expectedOutput = (VarInteger (Var [(14, atbt), (18, afbt), (22, atbf), (26, afbf)]))
    assertEqual "Soma lista [x, x+1, x+2, x+3]" expectedOutput output

testSomaLista :: Test
testSomaLista = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Lista-inputList.lng" inputList
    let expectedOutput = (VarInteger (Var [(13, atbt), (8, afbt), (9, atbf), (10, afbf)]))
    assertEqual "Soma lista" expectedOutput output

testSomaParSimples :: Test
testSomaParSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Pares.lng" inputInt
    let expectedOutput = VarInteger (Var [(22, atbt), (23, afbt),  (24, atbf), (25, afbf)])
    assertEqual "Soma par (x, 20)" expectedOutput output

testSomaPar :: Test
testSomaPar = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Pares-inputPair.lng" inputPair
    let expectedOutput = (VarInteger (Var [(10, atbt), ( 6, afbt),  (4, atbf), (7, afbf)]))
    assertEqual "Soma par (x, y)" expectedOutput output

testBool :: Test
testBool = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Bool.lng" inputBool
    let expectedOutput = VarBool (Var [(True, atbt), (False, afbt),  (False, atbf), (False, afbf)])
    assertEqual "Bool true" expectedOutput output


testConcatSimples :: Test
testConcatSimples = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Concat.lng" inputString
    let expectedOutput = VarString (Var [
            ("abc_concatenated", atbt), 
            ("def_concatenated", afbt), 
            ("ghi_concatenated", atbf), 
            ("jkl_concatenated", afbf)])
    assertEqual "concat x = x_concatenated" expectedOutput output

testConcatLista :: Test
testConcatLista = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Concat-inputList.lng" inputListString
    let expectedOutput = VarString (Var [
            ("abcdefghi", atbt), 
            ("123456789", afbt), 
            ("hello_world", atbf), 
            ("this_is_a_test", afbf)])
    assertEqual "Concat lista" expectedOutput output

testPolymorphicPair :: Test
testPolymorphicPair = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Polymorphic-pair.lng" inputPolymorphicPair
    let expectedOutput = (VarPair (VarBool (Var [(True, atbt), (False, afbt),  (False, atbf), (False, afbf)]), VarInteger (Var [(8, atbt), ( 5, afbt),  (0, atbf), (1, afbf)])))
    assertEqual "Invert pair" expectedOutput output


testPolymorphicList :: Test
testPolymorphicList = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Polymorphic-list.lng" inputPolymorphicList
    let expectedOutput = (VarInteger (Var [(3, atbt), (3, afbt), (3, atbf), (3, afbf)]))
    assertEqual "length list" (show expectedOutput) (show output)

-- TODO fix error 
-- expected: "VarInteger {int = {(1,DDNode {unDDNode = 0x00007fe0d600ad40}), (3,DDNode {unDDNode = 0x00007fe0d600ad61}), (2,DDNode {unDDNode = 0x00007fe0d600ace1})}}"
--  but got: "VarInteger {int = {(3,DDNode {unDDNode = 0x00007fe0d600ac20})}}"
testPolymorphicListIncomplete :: Test
testPolymorphicListIncomplete = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/Polymorphic-list.lng" inputPolymorphicListIncomplete
    let expectedOutput = (VarInteger (Var [(1, atbt), (3, afbt), (3, atbf), (2, afbf)]))
    assertEqual "length list" (show expectedOutput) (show output)

varTestSuite :: Test
varTestSuite = TestList [ TestLabel "Var testSimples" testSimples
                         , TestLabel "Var testFibonacci" testFibonacci
                         , TestLabel "Var testFatorial" testFatorial
                         , TestLabel "Var testSomaListaSimples" testSomaListaSimples
                         , TestLabel "Var testSomaLista" testSomaLista
                         , TestLabel "Var testSomaParSimples" testSomaParSimples
                         , TestLabel "Var testSomaPar" testSomaPar
                         , TestLabel "Var testBool" testBool
                         , TestLabel "Var testConcatSimples" testConcatSimples
                         , TestLabel "Var testConcatLista" testConcatLista
                         , TestLabel "Var testPolymorphicPair" testPolymorphicPair
                         , TestLabel "Var testPolymorphicList" testPolymorphicList
                         , TestLabel "Var testPolymorphicListIncomplete" testPolymorphicListIncomplete
                        ]