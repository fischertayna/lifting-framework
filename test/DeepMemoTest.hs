module DeepMemoTest where

import Test.HUnit
import Language.DeepMemo.Interpreter
import Language.DeepMemo.Driver
import Variability.VarTypes (Prop, VarValor(..), Var (Var), apply, mkBDDVar, notBDD, ttPC, (/\), (\/), (|||))
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
inputPair = VarPair (VarInteger (Var [(8, atbt), ( 5, afbt),  (0, atbf), (1, afbf)]), VarInteger (Var [(2, atbt), ( 1, afbt),  (4, atbf), (6, afbf)]))


testSimples :: Test
testSimples = TestCase $ do
    output <- processFile (executeProg "conditional") "src/Language/Examples/Simple.lng" inputInt
    let expectedValor = (VarInteger (Var [(6, atbt), (7, afbt), (8, atbf), (9, afbf)]))
    let expectedMem = [ ( [ VarInteger (Var [(2, atbt), (3, afbt), (4, atbf), (5, afbf)] ) ], 
                            VarInteger (Var [(3, atbt), (4, afbt), (5, atbf), (6, afbf)] ))]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Simple Conditional (3 + (if x then x+1 else x-1))" expectedOutput (output)

testFibonacci :: Test
testFibonacci = TestCase $ do
    output <- processFile (executeProg "fib") "src/Language/Examples/Fibonacci.lng" inputInt
    let expectedValor = (VarInteger (Var [(8, afbf), (5, atbf), (3, afbt), (2, atbt)]))
    putStrLn ("\n Memo for testFibonacci: " ++ show (snd output))
    assertEqual "Fibonacci x" expectedValor (fst output)

-- testFatorial :: Test
-- testFatorial = TestCase $ do
--     output <- processFile (executeProg "fat") "src/Language/Examples/Fatorial.lng" inputInt
--     let expectedValor = (VarInteger (Var [(2, atbt), (6, afbt), (24, atbf), (120, afbf)]))
--     let expectedMem = [([ValorInt 5], ValorInt 120),
--                        ([ValorInt 4], ValorInt 24),
--                        ([ValorInt 3], ValorInt 6),
--                        ([ValorInt 2], ValorInt 2),
--                        ([ValorInt 1], ValorInt 1),
--                        ([ValorInt 0], ValorInt 1)]
--     let expectedOutput = (expectedValor, expectedMem)
--     assertEqual "Fatorial x" expectedOutput output

-- testSomaListaSimples :: Test
-- testSomaListaSimples = TestCase $ do
--     output <- processFile (executeProg "soma") "src/Language/Examples/Lista.lng" inputInt
--     let expectedValor = (VarInteger (Var [(14, atbt), (18, afbt), (22, atbf), (26, afbf)]))
--     let expectedMem = [([ValorList [ValorInt 5, ValorInt 6, ValorInt 7, ValorInt 8]], ValorInt 26),
--                           ([ValorList [ValorInt 6, ValorInt 7, ValorInt 8]], ValorInt 21),
--                           ([ValorList [ValorInt 7, ValorInt 8]], ValorInt 15),
--                           ([ValorList [ValorInt 8]], ValorInt 8),
--                           ([ValorList [ValorInt 4, ValorInt 5, ValorInt 6, ValorInt 7]], ValorInt 22),
--                           ([ValorList [ValorInt 5, ValorInt 6, ValorInt 7]], ValorInt 18),
--                           ([ValorList [ValorInt 6, ValorInt 7]], ValorInt 13),
--                           ([ValorList [ValorInt 7]], ValorInt 7),
--                           ([ValorList [ValorInt 3, ValorInt 4, ValorInt 5, ValorInt 6]], ValorInt 18),
--                           ([ValorList [ValorInt 4, ValorInt 5, ValorInt 6]], ValorInt 15),
--                           ([ValorList [ValorInt 5, ValorInt 6]], ValorInt 11),
--                           ([ValorList [ValorInt 6]], ValorInt 6),
--                           ([ValorList [ValorInt 2, ValorInt 3, ValorInt 4, ValorInt 5]], ValorInt 14),
--                           ([ValorList [ValorInt 3, ValorInt 4, ValorInt 5]], ValorInt 12),
--                           ([ValorList [ValorInt 4, ValorInt 5]], ValorInt 9),
--                           ([ValorList [ValorInt 5]], ValorInt 5),
--                           ([ValorList []], ValorInt 0)]
--     let expectedOutput = (expectedValor, expectedMem)
--     assertEqual "Soma lista [x, x+1, x+2, x+3]" expectedOutput output

-- testSomaLista :: Test
-- testSomaLista = TestCase $ do
--     output <- processFile (executeProg "soma") "src/Language/Examples/Lista-inputList.lng" inputList
--     let expectedValor = (VarInteger (Var [(13, atbt), (8, afbt), (9, atbf), (10, afbf)]))
--     let expectedMem = [([ValorList [ValorInt 1, ValorInt 6, ValorInt 3]], ValorInt 10),
--                           ([ValorList [ValorInt 6, ValorInt 3]], ValorInt 9),
--                           ([ValorList [ValorInt 0, ValorInt 4, ValorInt 5]], ValorInt 9),
--                           ([ValorList [ValorInt 4, ValorInt 5]], ValorInt 9),
--                           ([ValorList [ValorInt 5]], ValorInt 5),
--                           ([ValorList [ValorInt 5, ValorInt 1, ValorInt 2]], ValorInt 8),
--                           ([ValorList [ValorInt 1, ValorInt 2]], ValorInt 3),
--                           ([ValorList [ValorInt 2]], ValorInt 2),
--                           ([ValorList [ValorInt 8, ValorInt 2, ValorInt 3]], ValorInt 13),
--                           ([ValorList [ValorInt 2, ValorInt 3]], ValorInt 5),
--                           ([ValorList [ValorInt 3]], ValorInt 3),
--                           ([ValorList []], ValorInt 0)]
--     let expectedOutput = (expectedValor, expectedMem)
--     assertEqual "Soma lista" expectedOutput output

-- testSomaParSimples :: Test
-- testSomaParSimples = TestCase $ do
--     output <- processFile (executeProg "soma") "src/Language/Examples/Pares.lng" inputInt
--     let expectedValor = VarInteger (Var [(22, atbt), (23, afbt),  (24, atbf), (25, afbf)])
--     let expectedMem = [([ValorPair (ValorInt 5, ValorInt 20)], ValorInt 25),
--                           ([ValorPair (ValorInt 4, ValorInt 20)], ValorInt 24),
--                           ([ValorPair (ValorInt 3, ValorInt 20)], ValorInt 23),
--                           ([ValorPair (ValorInt 2, ValorInt 20)], ValorInt 22)]
--     let expectedOutput = (expectedValor, expectedMem)
--     assertEqual "Soma par (x, 20)" expectedOutput output

-- testSomaPar :: Test
-- testSomaPar = TestCase $ do
--     output <- processFile (executeProg "soma") "src/Language/Examples/Pares-inputPair.lng" inputPair
--     let expectedValor = (VarInteger (Var [(10, atbt), ( 6, afbt),  (4, atbf), (7, afbf)]))
--     let expectedMem = [([ValorPair (ValorInt 1, ValorInt 6)], ValorInt 7),
--                           ([ValorPair (ValorInt 0, ValorInt 4)], ValorInt 4),
--                           ([ValorPair (ValorInt 5, ValorInt 1)], ValorInt 6),
--                           ([ValorPair (ValorInt 8, ValorInt 2)], ValorInt 10)]
--     let expectedOutput = (expectedValor, expectedMem)
--     assertEqual "Soma par (x, y)" expectedOutput output

-- testBool :: Test
-- testBool = TestCase $ do
--     output <- processFile (executeProg "or") "src/Language/Examples/Bool.lng" inputBool
--     let expectedValor = VarBool (Var [(True, atbt), (False, afbt),  (False, atbf), (False, afbf)])
--     let expectedMem = [([ValorBool True, ValorBool False], ValorBool True),
--                           ([ValorBool False, ValorBool True], ValorBool True)]
--     let expectedOutput = (expectedValor, expectedMem)
--     assertEqual "Bool true" expectedOutput output

-- testConcatSimples :: Test
-- testConcatSimples = TestCase $ do
--     output <- processFile (executeProg "concat") "src/Language/Examples/Concat.lng" inputString
--     let expectedValor = VarString (Var [
            -- ("abc_concatenated", atbt), 
            -- ("def_concatenated", afbt), 
            -- ("ghi_concatenated", atbf), 
            -- ("jkl_concatenated", afbf)])
--     let expectedMem = [([ValorStr "jkl", ValorStr "_concatenated"], ValorStr "jkl_concatenated"),
--                           ([ValorStr "ghi", ValorStr "_concatenated"], ValorStr "ghi_concatenated"),
--                           ([ValorStr "def", ValorStr "_concatenated"], ValorStr "def_concatenated"),
--                           ([ValorStr "abc", ValorStr "_concatenated"], ValorStr "abc_concatenated")]
--     let expectedOutput = (expectedValor, expectedMem)
--     assertEqual "concat x = x_concatenated" expectedOutput output

-- testConcatLista :: Test
-- testConcatLista = TestCase $ do
--     output <- processFile (executeProg "concat") "src/Language/Examples/Concat-inputList.lng" inputListString
--     let expectedValor = VarString (Var [
            -- ("abcdefghi", atbt), 
            -- ("123456789", afbt), 
            -- ("hello_world", atbf), 
            -- ("this_is_a_test", afbf)])
--     let expectedMem = [ ([ValorList [ValorStr "this_", ValorStr "is_", ValorStr "a_", ValorStr "test"]], ValorStr "this_is_a_test")
--                          , ([ValorList [ValorStr "is_", ValorStr "a_", ValorStr "test"]], ValorStr "is_a_test")
--                          , ([ValorList [ValorStr "a_", ValorStr "test"]], ValorStr "a_test")
--                          , ([ValorList [ValorStr "test"]], ValorStr "test")
--                          , ([ValorList [ValorStr "hello", ValorStr "_", ValorStr "world"]], ValorStr "hello_world")
--                          , ([ValorList [ValorStr "_", ValorStr "world"]], ValorStr "_world")
--                          , ([ValorList [ValorStr "world"]], ValorStr "world")
--                          , ([ValorList [ValorStr "123", ValorStr "456", ValorStr "789"]], ValorStr "123456789")
--                          , ([ValorList [ValorStr "456", ValorStr "789"]], ValorStr "456789")
--                          , ([ValorList [ValorStr "789"]], ValorStr "789")
--                          , ([ValorList [ValorStr "abc", ValorStr "def", ValorStr "ghi"]], ValorStr "abcdefghi")
--                          , ([ValorList [ValorStr "def", ValorStr "ghi"]], ValorStr "defghi")
--                          , ([ValorList [ValorStr "ghi"]], ValorStr "ghi")
--                          , ([ValorList []], ValorStr "")
--                          ]
--     let expectedOutput = (expectedValor, expectedMem)
--     assertEqual "Concat lista" expectedOutput output

deepMemoTestSuite :: Test
deepMemoTestSuite = TestList [ TestLabel "Var testFibonacci" testFibonacci
                            -- TestLabel "Var testSimples" testSimples
                            -- , TestLabel "Var testSimples False" testSimplesCondFalse
                        --  , TestLabel "Var testFibonacci" testFibonacci
                        --  , TestLabel "Var testFatorial" testFatorial
                        --  , TestLabel "Var testSomaListaSimples" testSomaListaSimples
                        --  , TestLabel "Var testSomaLista" testSomaLista
                        --  , TestLabel "Var testSomaParSimples" testSomaParSimples
                        --  , TestLabel "Var testSomaPar" testSomaPar
                        --  , TestLabel "Var testBool" testBool
                        --  , TestLabel "Var testConcatSimples" testConcatSimples
                        --  , TestLabel "Var testConcatLista" testConcatLista
                            ]