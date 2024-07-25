module DeepMemoTest where

import Test.HUnit
import Language.DeepMemo.Interpreter
import Language.DeepMemo.Driver
import Variability.VarTypes (Prop, VarValor(..), Var (Var), apply, mkBDDVar, notBDD, ttPC, ffPC, tt, ff, (/\), (\/), (|||))
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

-- substitutions :: [(String, String)]
-- substitutions =
--     [ (show atbt, " A && B")
--     , (show afbt, " ~A && B")
--     , (show atbf, " A && ~B")
--     , (show afbf, " ~A && ~B")
--     , (show tt, " True")
--     , (show ff, " False")
--     , (show ttPC, " True")
--     , (show ffPC, " False")
--     ]

substitutions :: [(String, String)]
substitutions =
    [ (show atbt, " atbt")
    , (show afbt, " afbt")
    , (show atbf, " atbf")
    , (show afbf, " afbf")
    , (show tt, " tt")
    , (show ff, " ff")
    , (show ttPC, " ttPC")
    , (show ffPC, " ffPC")
    ]

inputInt, inputHighInt, inputBool, inputString, inputList, inputPair :: VarValor
inputInt = VarInteger (Var [(2, atbt), (3, afbt),  (4, atbf), (5, afbf)])
inputHighInt = VarInteger (Var [(25, atbt), (50, afbt),  (75, atbf), (100, afbf)])
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
    output <- processFile (executeProg "conditional") "src/Language/Examples/Simple.lng" inputInt
    let expectedValor = (VarInteger (Var [(6, atbt), (7, afbt), (8, atbf), (9, afbf)]))
    let expectedMem = [ ( [ VarInteger (Var [(2, atbt), (3, afbt), (4, atbf), (5, afbf)] ) ], 
                            VarInteger (Var [(3, atbt), (4, afbt), (5, atbf), (6, afbf)] ))]
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Simple Conditional (3 + (if x then x+1 else x-1))" expectedOutput output

testFibonacci :: Test
testFibonacci = TestCase $ do
    output <- processFile (executeProg "fib") "src/Language/Examples/Fibonacci.lng" inputInt
    let expectedValor = (VarInteger (Var [(5, afbf), (3, atbf), (2, afbt), (1, atbt)]))
    putStrLn ("\n Mem for testFibonacci: " ++ (substitute (show (snd output)) substitutions))
    assertEqual "Fibonacci x" expectedValor (fst output)

testHighFibonacci :: Test
testHighFibonacci = TestCase $ do
    output <- processFile (executeProg "fib") "src/Language/Examples/Fibonacci.lng" inputHighInt
    let expectedValor = (VarInteger (Var [(75025, atbt), (12586269025, afbt), (2111485077978050, atbf), (354224848179261915075, afbf)]))
    -- putStrLn ("\n Mem for High Fibonacci: " ++ (substitute (show (snd output)) substitutions))
    assertEqual "Fibonacci high x" expectedValor (fst output)

testFatorial :: Test
testFatorial = TestCase $ do
    output <- processFile (executeProg "fat") "src/Language/Examples/Fatorial.lng" inputInt
    let expectedValor = (VarInteger (Var [(2, atbt), (6, afbt), (24, atbf), (120, afbf)]))
    let expectedMem = [([VarInteger (Var [(2, atbt), (3, afbt), (4, atbf), (5, afbf)])],
                            VarInteger (Var [(2, atbt), (6, afbt), (24, atbf), (120, afbf)])),
                        ([VarInteger (Var [(1, atbt), (2, afbt), (3, atbf), (4, afbf)])],
                            VarInteger (Var [(1, atbt), (2, afbt), (6, atbf), (24, afbf)])),
                        ([VarInteger (Var [(0, atbt), (1, afbt), (2, atbf), (3, afbf)])],
                            VarInteger (Var [(1, afbt), (2, atbf), (6, afbf), (1, atbt)])),
                        ([VarInteger (Var [(0, afbt), (1, atbf), (2, afbf)])],
                            VarInteger (Var [(1, atbf), (2, afbf), (1, afbt)])),
                        ([VarInteger (Var [(0, atbf), (1, afbf)])],
                            VarInteger (Var [(1, afbf), (1, atbf)])),
                        ([VarInteger (Var [(0, afbf)])],
                            VarInteger (Var [(1, tt)]))]
    -- putStrLn ("\n Mem for Fatorial: " ++ (substitute (show (snd output)) substitutions))
    let expectedOutput = (expectedValor, expectedMem)
    assertEqual "Fatorial x" expectedOutput output

testSomaListaSimples :: Test
testSomaListaSimples = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Lista.lng" inputInt
    let expectedValor = (VarInteger (Var [(14, atbt), (18, afbt), (22, atbf), (26, afbf)]))
    let expectedMem = [ (   [VarList [
                                VarInteger (Var [(2, atbt), (3, afbt), (4, atbf), (5, afbf)]),
                                VarInteger (Var [(3, atbt), (4, afbt), (5, atbf), (6, afbf)]),
                                VarInteger (Var [(4, atbt), (5, afbt), (6, atbf), (7, afbf)]),
                                VarInteger (Var [(5, atbt), (6, afbt), (7, atbf), (8, afbf)])]],
                            VarInteger (Var [(14, atbt), (18, afbt), (22, atbf), (26, afbf)])),
                        (   [VarList [
                                VarInteger (Var [(3, atbt), (4, afbt), (5, atbf), (6, afbf)]),
                                VarInteger (Var [(4, atbt), (5, afbt), (6, atbf), (7, afbf)]),
                                VarInteger (Var [(5, atbt), (6, afbt), (7, atbf), (8, afbf)])]],
                            VarInteger (Var [(12, atbt), (15, afbt), (18, atbf), (21, afbf)])),
                        (   [VarList [
                                VarInteger (Var [(4, atbt), (5, afbt), (6, atbf), (7, afbf)]),
                                VarInteger (Var [(5, atbt), (6, afbt), (7, atbf), (8, afbf)])]],
                            VarInteger (Var [(9, atbt), (11, afbt), (13, atbf), (15, afbf)])),
                        (   [VarList [VarInteger (Var [(5, atbt), (6, afbt), (7, atbf), (8, afbf)])]],
                            VarInteger (Var [(5, atbt), (6, afbt), (7, atbf), (8, afbf)])),
                        (   [VarList []],
                            VarInteger (Var [(0, tt)]))]
    let expectedOutput = (expectedValor, expectedMem)
    -- putStrLn ("\n Mem for Soma lista simples: " ++ (substitute (show (snd output)) substitutions))
    assertEqual "Soma lista [x, x+1, x+2, x+3]" expectedOutput output

testSomaLista :: Test
testSomaLista = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Lista-inputList.lng" inputList
    let expectedValor = (VarInteger (Var [(13, atbt), (8, afbt), (9, atbf), (10, afbf)]))
    -- let expectedMem = [(    [VarList [
    --                             VarInteger (Var [(8, atbt), (5, afbt), (0, atbf), (1, afbf)]),
    --                             VarInteger (Var [(2, atbt), (1, afbt), (4, atbf), (6, afbf)]),
    --                             VarInteger (Var [(3,DDNode {unDDNode = 0x00007fef55824960}), (2, afbt), (5, atbf)])]],
    --                         VarInteger (Var [(13, atbt), (8, afbt), (9, atbf), (10, afbf)])),
    --                     (   [VarList [
    --                             VarInteger (Var [(2, atbt), (1, afbt), (4, atbf), (6, afbf)]),
    --                             VarInteger (Var [(3,DDNode {unDDNode = 0x00007fef55824960}), (2, afbt), (5, atbf)])]],
    --                         VarInteger (Var [(5, atbt), (3, afbt), (9, atbf), (9, afbf)])),
    --                     (   [VarList [
    --                             VarInteger (Var [(3,DDNode {unDDNode = 0x00007fef55824960}), (2, afbt), (5, atbf)])]],
    --                             VarInteger (Var [(3,DDNode {unDDNode = 0x00007fef55824960}), (2, afbt), (5, atbf)])),
    --                     (   [VarList []],
    --                         VarInteger (Var [(0, tt)]))]
    -- let expectedOutput = (expectedValor, expectedMem)
    putStrLn ("\n Mem for Soma lista: " ++ (substitute (show (snd output)) substitutions))
    assertEqual "Soma lista" expectedValor (fst output)

testSomaParSimples :: Test
testSomaParSimples = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Pares.lng" inputInt
    let expectedValor = VarInteger (Var [(22, atbt), (23, afbt),  (24, atbf), (25, afbf)])
    let expectedMem = [([VarPair (VarInteger (Var [(2, atbt), (3, afbt), (4, atbf), (5, afbf)]),
                                  VarInteger (Var [(20, tt)]))],
                        VarInteger (Var [(22, atbt), (23, afbt), (24, atbf), (25, afbf)]))]
    let expectedOutput = (expectedValor, expectedMem)
    -- putStrLn ("\n Mem for Soma par (x, 20): " ++ (substitute (show (snd output)) substitutions))
    assertEqual "Soma par (x, 20)" expectedOutput output

testSomaPar :: Test
testSomaPar = TestCase $ do
    output <- processFile (executeProg "soma") "src/Language/Examples/Pares-inputPair.lng" inputPair
    let expectedValor = (VarInteger (Var [(10, atbt), ( 6, afbt),  (4, atbf), (7, afbf)]))
    let expectedMem = [([VarPair (VarInteger (Var [(8, atbt), (5, afbt), (0, atbf), (1, afbf)]),
                                  VarInteger (Var [(2, atbt), (1, afbt), (4, atbf), (6, afbf)]))],
                        VarInteger (Var [(10, atbt), (6, afbt), (4, atbf), (7, afbf)]))]
    let expectedOutput = (expectedValor, expectedMem)
    -- putStrLn ("\n Mem for Soma par (x, y): " ++ (substitute (show (snd output)) substitutions))
    assertEqual "Soma par (x, y)" expectedOutput output

testBool :: Test
testBool = TestCase $ do
    output <- processFile (executeProg "or") "src/Language/Examples/Bool.lng" inputBool
    let expectedValor = VarBool (Var [(True, atbt), (False, afbt),  (False, atbf), (False, afbf)])
    -- let expectedMem = [([VarBool (Var [(False, atbt), (True,DDNode {unDDNode = 0x00007fd3b5008341})]),
    --                     VarBool (Var [(True, atbt), (False,DDNode {unDDNode = 0x00007fd3b5008341})])],
    --                     VarBool (Var [(True, tt)]))]
    putStrLn ("\n Mem for Bool " ++ (substitute (show (snd output)) substitutions))
    assertEqual "Bool" expectedValor (fst output)

testConcatSimples :: Test
testConcatSimples = TestCase $ do
    output <- processFile (executeProg "concat") "src/Language/Examples/Concat.lng" inputString
    let expectedValor = VarString (Var [
            ("abc_concatenated", atbt), 
            ("def_concatenated", afbt), 
            ("ghi_concatenated", atbf), 
            ("jkl_concatenated", afbf)])
    let expectedMem = [([   VarString (Var [("abc", atbt), ("def", afbt), ("ghi", atbf), ("jkl", afbf)]),
                            VarString (Var [("_concatenated", tt)])],
                        VarString (Var [("abc_concatenated", atbt), ("def_concatenated", afbt), ("ghi_concatenated", atbf), ("jkl_concatenated", afbf)]))]
    let expectedOutput = (expectedValor, expectedMem)
    -- putStrLn ("\n Memo for x = x_concatenated " ++ (substitute (show (snd output)) substitutions))
    assertEqual "concat x = x_concatenated" expectedOutput output

testConcatLista :: Test
testConcatLista = TestCase $ do
    output <- processFile (executeProg "concat") "src/Language/Examples/Concat-inputList.lng" inputListString
    let expectedValor = VarString (Var [
            ("abcdefghi", atbt), 
            ("123456789", afbt), 
            ("hello_world", atbf), 
            ("this_is_a_test", afbf)])
    let expectedMem = [([VarList [
                                    VarString (Var [("abc", atbt), ("123", afbt), ("hello", atbf), ("this_", afbf)]),
                                    VarString (Var [("def", atbt), ("456", afbt), ("_", atbf), ("is_", afbf)]),
                                    VarString (Var [("ghi", atbt), ("789", afbt), ("world", atbf), ("a_test", afbf)])
                                ]],
                        VarString (Var [("abcdefghi", atbt), ("123456789", afbt), ("hello_world", atbf), ("this_is_a_test", afbf)])),
                       ([VarList [
                                    VarString (Var [("def", atbt), ("456", afbt), ("_", atbf), ("is_", afbf)]),
                                    VarString (Var [("ghi", atbt), ("789", afbt), ("world", atbf), ("a_test", afbf)])
                                ]],
                        VarString (Var [("defghi", atbt), ("456789", afbt), ("_world", atbf), ("is_a_test", afbf)])),
                       ([VarList [VarString (Var [("ghi", atbt), ("789", afbt), ("world", atbf), ("a_test", afbf)])]],
                        VarString (Var [("ghi", atbt), ("789", afbt), ("world", atbf), ("a_test", afbf)])),
                       ([VarList []],
                        VarString (Var [("", tt)]))]
    let expectedOutput = (expectedValor, expectedMem)
    -- putStrLn ("\n Memo for Concat lista " ++ (substitute (show (snd output)) substitutions))
    assertEqual "Concat lista" expectedOutput output

testPolymorphicPair :: Test
testPolymorphicPair = TestCase $ do
    output <- processFile (executeProg "invert") "src/Language/Examples/Polymorphic-pair.lng" inputPolymorphicPair
    let expectedValor = (VarPair (VarBool (Var [(True, atbt), (False, afbt),  (False, atbf), (False, afbf)]), VarInteger (Var [(8, atbt), ( 5, afbt),  (0, atbf), (1, afbf)])))
    -- let expectedMem = [([VarPair {pair = (VarInteger {int = {(8, atbt), (5, afbt), (0, atbf), (1, afbf)}},VarBool {bool = {(True, atbt), (False,DDNode {unDDNode = 0x00007fdde1008341})}})}],VarPair {pair = (VarBool {bool = {(True, atbt), (False,DDNode {unDDNode = 0x00007fdde1008341})}},VarInteger {int = {(8, atbt), (5, afbt), (0, atbf), (1, afbf)}})})]
    putStrLn ("\n Memo for invert PolymorphicPair " ++ (substitute (show (snd output)) substitutions))
    assertEqual "invert pair" expectedValor (fst output)


-- TODO fix error 
-- expected: VarInteger {int = {(3,DDNode {unDDNode = 0x00007f96cb81da20})}}
-- but got: VarInteger {int = {(3,DDNode {unDDNode = 0x00007f96cb81da20})}}
testPolymorphicList :: Test
testPolymorphicList = TestCase $ do
    output <- processFile (executeProg "length") "src/Language/Examples/Polymorphic-list.lng" inputPolymorphicList
    let expectedValor = (VarInteger (Var [(3, atbt), (3, afbt), (3, atbf), (3, afbf)]))
    -- let expectedMem = [([VarList {list = [VarInteger {int = {(8, atbt), (5, afbt), (0, atbf), (1, afbf)}},VarBool {bool = {(True, atbt), (False,DDNode {unDDNode = 0x00007fd0ae81b941})}},VarString {str = {("abc", atbt), ("def", afbt), ("ghi", atbf), ("jkl", afbf)}}]}],VarInteger {int = {(3, tt)}}),([VarList {list = [VarBool {bool = {(True, atbt), (False,DDNode {unDDNode = 0x00007fd0ae81b941})}},VarString {str = {("abc", atbt), ("def", afbt), ("ghi", atbf), ("jkl", afbf)}}]}],VarInteger {int = {(2, tt)}}),([VarList {list = [VarString {str = {("abc", atbt), ("def", afbt), ("ghi", atbf), ("jkl", afbf)}}]}],VarInteger {int = {(1, tt)}}),([VarList {list = []}],VarInteger {int = {(0, tt)}})]
    putStrLn ("\n Memo for length PolymorphicList " ++ (substitute (show (snd output)) substitutions))
    assertEqual "length PolymorphicList" (show expectedValor) (show (fst output))

-- TODO fix error 
-- expected: "VarInteger {int = {(1,DDNode {unDDNode = 0x00007fe0d600ad40}), (3,DDNode {unDDNode = 0x00007fe0d600ad61}), (2,DDNode {unDDNode = 0x00007fe0d600ace1})}}"
--  but got: "VarInteger {int = {(3,DDNode {unDDNode = 0x00007fe0d600ac20})}}"
testPolymorphicListIncomplete :: Test
testPolymorphicListIncomplete = TestCase $ do
    output <- processFile (executeProg "length") "src/Language/Examples/Polymorphic-list.lng" inputPolymorphicListIncomplete
    let expectedValor = (VarInteger (Var [(1, atbt), (3, afbt), (3, atbf), (2, afbf)]))
    -- let expectedMem = []
    putStrLn ("\n Memo for PolymorphicListIncomplete " ++ (substitute (show (snd output)) substitutions))
    assertEqual "length PolymorphicListIncomplete" expectedValor (fst output)

deepMemoTestSuite :: Test
deepMemoTestSuite = TestList [ TestLabel "DeepMemo testSimples" testSimples
                            , TestLabel "DeepMemo testFibonacci" testFibonacci
                            , TestLabel "DeepMemo testFibonacci" testHighFibonacci
                            , TestLabel "DeepMemo testFatorial" testFatorial
                            , TestLabel "DeepMemo testSomaListaSimples" testSomaListaSimples
                            , TestLabel "DeepMemo testSomaLista" testSomaLista
                            , TestLabel "DeepMemo testSomaParSimples" testSomaParSimples
                            , TestLabel "DeepMemo testSomaPar" testSomaPar
                            , TestLabel "DeepMemo testBool" testBool
                            , TestLabel "DeepMemo testConcatSimples" testConcatSimples
                            , TestLabel "DeepMemo testConcatLista" testConcatLista
                            , TestLabel "DeepMemo testPolymorphicPair" testPolymorphicPair
                            , TestLabel "DeepMemo testPolymorphicList" testPolymorphicList
                            , TestLabel "DeepMemo testPolymorphicListIncomplete" testPolymorphicListIncomplete
                            ]