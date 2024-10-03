module VarReachingDefinitionsTest where

import Language.VInterpreter.Interpreter
import Language.VInterpreter.Driver
import Variability.VarTypes (Prop, VarValor(..), Var (Var), apply, mkBDDVar, notBDD, ttPC, ffPC, tt, ff, (/\), (\/), (|||))

import Language.Frontend.ErrM
import Helper

import Test.HUnit
import System.Timeout (timeout)
import Control.Exception (evaluate)

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
    , (show propA, " A")
    , (show (notBDD propA), " ~A")
    ]

-- ex1, ex2, ex3, ex4, factorialProg :: VarValor
ex1 :: VarValor

-- x = 1;      1
-- y = 2;      2
-- y = 4;      3
-- z = x + y;  4

-- x = 1;  tt    1
-- y = 2;  tt    2
-- y = 4;  tt    3
-- z = x + y;  tt 4
ex1 = VarPair(
        VarString (Var [("SEQ", ttPC)]),
        VarPair ( 
            VarPair (
                VarString (Var [("ASGN", ttPC)]),
                VarPair (
                    VarString (Var [("1", ttPC)]),
                    VarPair (
                        VarString (Var [("x", ttPC)]),
                        VarPair(
                            VarString (Var [("INT", ttPC)]),
                            VarString (Var [("1", ttPC)])
                        )
                    )
                )
            ), -- x = 1;  tt    1
            VarPair(
                VarString (Var [("SEQ", ttPC)]), -- Should it be A??
                VarPair ( 
                    VarPair (
                        VarString (Var [("ASGN", ttPC)]),
                        VarPair (
                            VarString (Var [("2", ttPC)]),
                            VarPair (
                                VarString (Var [("y", ttPC)]),
                                VarPair(
                                    VarString (Var [("INT", ttPC)]),
                                    VarString (Var [("1", ttPC)])
                                )
                            )
                        )
                    ), -- y = 2;    2
                    VarPair(
                        VarString (Var [("SEQ", ttPC)]), 
                        VarPair(
                            VarPair (
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("3", ttPC)]),
                                    VarPair (
                                        VarString (Var [("y", ttPC)]),
                                        VarPair(
                                            VarString (Var [("INT", ttPC)]),
                                            VarString (Var [("4", ttPC)])
                                        )
                                    )
                                )
                            ), -- y = 4;    3
                            VarPair(
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("4", ttPC)]),
                                    VarPair (
                                        VarString (Var [("z", ttPC)]),
                                        VarPair (
                                            VarString (Var [("ADD", ttPC)]),
                                            VarPair (
                                                VarPair (
                                                    VarString (Var [("VAR", ttPC)]),
                                                    VarString (Var [("x", ttPC)])
                                                ),
                                                VarPair (
                                                    VarString (Var [("VAR", ttPC)]),
                                                    VarString (Var [("y", ttPC)])
                                                )
                                            )   
                                        )
                                    )
                                )
                            ) -- z = x + y;  tt 4
                        )
                    )
                )
            )
        )
    )


-- x = 1;      1
-- #IFDEF A
-- y = 2;      2
-- #ELSE
-- y = 4;      3
-- #ENDIF
-- z = x + y;  4

-- x = 1;  tt    1
-- y = 2;  A  skip ~A   2
-- y = 4;  ~A  skip A   3
-- z = x + y;  tt 4
ex2 = VarPair(
        VarString (Var [("SEQ", ttPC)]),
        VarPair ( 
            VarPair (
                VarString (Var [("ASGN", ttPC)]),
                VarPair (
                    VarString (Var [("1", ttPC)]),
                    VarPair (
                        VarString (Var [("x", ttPC)]),
                        VarPair(
                            VarString (Var [("INT", ttPC)]),
                            VarString (Var [("1", ttPC)])
                        )
                    )
                )
            ), -- x = 1;  tt    1
            VarPair(
                VarString (Var [("SEQ", ttPC)]), -- Should it be A??
                VarPair ( 
                    VarPair (
                        VarString (Var [("ASGN", propA), ("SKIP", notBDD propA)]),
                        VarPair (
                            VarString (Var [("2", propA), ("2", notBDD propA)]),
                            VarPair (
                                VarString (Var [("y", propA), ("DUMMY", notBDD propA)]),
                                VarPair(
                                    VarString (Var [("INT", propA), ("DUMMY", notBDD propA)]),
                                    VarString (Var [("1", propA), ("DUMMY", notBDD propA)])
                                )
                            )
                        )
                    ), -- y = 2;  A    2
                    VarPair(
                        VarString (Var [("SEQ", ttPC)]), -- Should it be ~A??
                        VarPair(
                            VarPair (
                                VarString (Var [("SKIP", propA), ("ASGN", notBDD propA)]),
                                VarPair (
                                    VarString (Var [("3", propA), ("3", notBDD propA)]),
                                    VarPair (
                                        VarString (Var [("DUMMY", propA), ("y", notBDD propA)]),
                                        VarPair(
                                            VarString (Var [("DUMMY", propA), ("INT", notBDD propA)]),
                                            VarString (Var [("DUMMY", propA), ("4", notBDD  propA)])
                                        )
                                    )
                                )
                            ), -- y = 4;  ~A    3
                            VarPair(
                                VarString (Var [("ASGN", ttPC)]),
                                VarPair (
                                    VarString (Var [("4", ttPC)]),
                                    VarPair (
                                        VarString (Var [("z", ttPC)]),
                                        VarPair (
                                            VarString (Var [("ADD", ttPC)]),
                                            VarPair (
                                                VarPair (
                                                    VarString (Var [("VAR", ttPC)]),
                                                    VarString (Var [("x", ttPC)])
                                                ),
                                                VarPair (
                                                    VarString (Var [("VAR", ttPC)]),
                                                    VarString (Var [("y", ttPC)])
                                                )
                                            )   
                                        )
                                    )
                                )
                            ) -- z = x + y;  tt 4
                        )
                    )
                )
            )
        )
    )

-- x = 1;  A || ~A    1
-- y = 2;  A    2
-- y = 4;  ~A    3
-- z = x + y;  A || ~A 4
ex3 = VarPair(
        VarString (Var [("SEQ", propA), ("SEQ", notBDD propA)]),
        VarPair ( 
            VarPair (
                VarString (Var [("ASGN", propA), ("ASGN", notBDD propA)]),
                VarPair (
                    VarString (Var [("1", propA), ("1", notBDD propA)]),
                    VarPair (
                        VarString (Var [("x", propA), ("x", notBDD propA)]),
                        VarPair(
                            VarString (Var [("INT", propA), ("INT", notBDD propA)]),
                            VarString (Var [("1", propA), ("1", notBDD propA)])
                        )
                    )
                )
            ), -- x = 1;  tt    1
            VarPair(
                VarString (Var [("SEQ", propA), ("SEQ", notBDD propA)]), -- Should it be A??
                VarPair ( 
                    VarPair (
                        VarString (Var [("ASGN", propA), ("SKIP", notBDD propA)]),
                        VarPair (
                            VarString (Var [("2", propA), ("2", notBDD propA)]),
                            VarPair (
                                VarString (Var [("y", propA), ("DUMMY", notBDD propA)]),
                                VarPair(
                                    VarString (Var [("INT", propA), ("DUMMY", notBDD propA)]),
                                    VarString (Var [("1", propA), ("DUMMY", notBDD propA)])
                                )
                            )
                        )
                    ), -- y = 2;  A    2
                    VarPair(
                        VarString (Var [("SEQ", propA), ("SEQ", notBDD propA)]), -- Should it be ~A??
                        VarPair(
                            VarPair (
                                VarString (Var [("ASGN", notBDD propA), ("SKIP", propA)]),
                                VarPair (
                                    VarString (Var [("3", notBDD propA), ("3", propA)]),
                                    VarPair (
                                        VarString (Var [("y", notBDD propA), ("DUMMY", propA)]),
                                        VarPair(
                                            VarString (Var [("INT", notBDD propA), ("DUMMY", propA)]),
                                            VarString (Var [("4",notBDD  propA), ("DUMMY", propA)])
                                        )
                                    )
                                )
                            ), -- y = 4;  ~A    3
                            VarPair(
                                VarString (Var [("ASGN", propA), ("ASGN", notBDD propA)]),
                                VarPair (
                                    VarString (Var [("4", propA), ("4", notBDD propA)]),
                                    VarPair (
                                        VarString (Var [("z", propA), ("z", notBDD propA)]),
                                        VarPair (
                                            VarString (Var [("ADD", propA), ("ADD", notBDD propA)]),
                                            VarPair (
                                                VarPair (
                                                    VarString (Var [("VAR", propA), ("VAR", notBDD propA)]),
                                                    VarString (Var [("x", propA), ("x", notBDD propA)])
                                                ),
                                                VarPair (
                                                    VarString (Var [("VAR", propA), ("VAR", notBDD propA)]),
                                                    VarString (Var [("y", propA), ("y", notBDD propA)])
                                                )
                                            )   
                                        )
                                    )
                                )
                            ) -- z = x + y;  tt 4
                        )
                    )
                )
            )
        )
    )
    
testElem :: Test
testElem = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/elem.lng" (VarPair (
                                                        VarInteger (Var [(1, propA), (2, notBDD propA)]),
                                                        VarList [VarBool (Var [(True, ttPC)]), VarString (Var [("2", ttPC)]), VarInteger (Var [(1, ttPC)])]
                                                    ))
    let expectedOutput = (VarInteger (Var [(1, propA), (0, notBDD propA)]))
    -- putStrLn ("\n elem: " ++ (substitute (show output) substitutions))
    assertEqual "elem" expectedOutput output

testAddUnique :: Test
testAddUnique = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/addUnique.lng" (VarPair (
                                                        VarInteger (Var [(1, propA), (2, notBDD propA)]),
                                                        VarList [VarBool (Var [(True, ttPC)]), VarString (Var [("2", ttPC)]), VarInteger (Var [(1, ttPC)])]
                                                    ))
    putStrLn ("\n addUnique: " ++ (substitute (show output) substitutions))
    let expectedOutput = (VarList [VarBool (Var [(True, ttPC)]), VarString (Var [("2", ttPC)]), VarInteger (Var [(1, ttPC)])])
    assertEqual "addUnique" expectedOutput output

-- testAddUniqueSame :: Test
-- testAddUniqueSame = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/addUnique.lng" (ValorPair(
--             ValorStr "b",
--             ValorList [ValorBool True, ValorStr "b", ValorInt 4]
--         ))
--     let expectedOutput = (ValorList [ValorBool True, ValorStr "b", ValorInt 4])
--     assertEqual "addUnique same" expectedOutput output

-- testUnion :: Test
-- testUnion = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/union.lng" (ValorPair(
--             ValorList [ValorStr "a", ValorStr "b", ValorInt 1],
--             ValorList [ValorBool True, ValorStr "b", ValorInt 4]
--         ))
--     let expectedOutput = (ValorList [ValorStr "a", ValorStr "b", ValorInt 1, ValorBool True, ValorInt 4])
--     assertEqual "union" expectedOutput output


testIsPair :: Test
testIsPair = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/isPair.lng" ex1
    let expectedOutput = (VarBool (Var [(True, ttPC)]))
    assertEqual "is ex1 pair" expectedOutput output

testIsEqual1 :: Test
testIsEqual1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/isEqual.lng" (VarPair (
                                                        VarInteger (Var [(1, ttPC)]),
                                                        VarInteger (Var [(1, ttPC)])
                                                    ))
    let expectedOutput = (VarBool (Var [(True, ttPC)]))
    assertEqual "is ex1 pair" expectedOutput output

testIsEqual2 :: Test
testIsEqual2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/isEqual.lng" (VarPair (
                                                        VarInteger (Var [(2, propA), (1, notBDD propA)]),
                                                        VarInteger (Var [(1, ttPC)])
                                                    ))
    let expectedOutput = (VarBool (Var [(False, propA), (True, notBDD propA)]))
    assertEqual "is ex1 pair" expectedOutput output

testIsEqual3 :: Test
testIsEqual3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/isEqual.lng" (VarPair (
                                                        VarInteger (Var [(1, propA), (2, notBDD propA)]),
                                                        VarInteger (Var [(1, propA), (3, notBDD propA)])
                                                    ))
    let expectedOutput = (VarBool (Var [(True, propA), (False, notBDD propA)]))
    assertEqual "is ex1 pair" expectedOutput output

testIsEqual4 :: Test
testIsEqual4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/isEqual.lng" (VarPair (
                                                        VarString (Var [("ASGN", propA), ("DUMMY", notBDD propA)]),
                                                        VarString (Var [("ASGN", ttPC)])
                                                    ))
    let expectedOutput = (VarBool (Var [(True, propA), (False, notBDD propA)]))
    assertEqual "is ex1 pair" expectedOutput output

testCountEx1 :: Test
testCountEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/Count-Asgns.lng" ex1
    let expectedOutput = (VarInteger (Var [(4, ttPC)]))
    assertEqual "Count Asgns ex1" expectedOutput output

testCountEx2 :: Test
testCountEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/Count-Asgns.lng" ex2
    let expectedOutput = (VarInteger (Var [(3, propA), (3, notBDD propA)]))
    assertEqual "Count Asgns ex2" expectedOutput output

testCountEx3 :: Test
testCountEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/Count-Asgns.lng" ex3
    let expectedOutput = (VarInteger (Var [(3, propA), (3, notBDD propA)]))
    -- putStrLn ("\n Flow out: " ++ (substitute (show output) substitutions))
    assertEqual "Count Asgns ex3" expectedOutput output

testInitEx1 :: Test
testInitEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/init.lng" ex1
    let expectedOutput = (VarString (Var [("1", ttPC)]))
    assertEqual "init ex1" expectedOutput output

testInitEx2 :: Test
testInitEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/init.lng" ex2
    let expectedOutput = (VarString (Var [("1", ttPC)]))
    assertEqual "init ex2" expectedOutput output

testInitEx3 :: Test
testInitEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/init.lng" ex3
    let expectedOutput = (VarString (Var [("1", ttPC)]))
    assertEqual "init ex3" expectedOutput output


testFinalEx1 :: Test
testFinalEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/final.lng" ex1
    let expectedOutput = (VarList [VarString (Var [("4", ttPC)])])
    assertEqual "final ex1" expectedOutput output

testFinalEx2 :: Test
testFinalEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/final.lng" ex2
    let expectedOutput = (VarList [VarString (Var [("4", ttPC)])])
    assertEqual "final ex2" expectedOutput output

testFinalEx3 :: Test
testFinalEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/final.lng" ex3
    let expectedOutput = (VarList [VarString (Var [("4", ttPC)])])
    -- putStrLn ("\n final expected ex3: " ++ (substitute (show expectedOutput) substitutions))
    -- putStrLn ("\n final out ex3: " ++ (substitute (show output) substitutions))
    assertEqual "final ex3" (show expectedOutput) (show output)

testFlowEx1 :: Test
testFlowEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/cfg.lng" ex1
    let expectedOutput = (VarList[VarPair(VarString (Var [("1", ttPC)]),VarString (Var [("2", ttPC)])),
                                VarPair(VarString (Var [("2", ttPC)]),VarString (Var [("3", ttPC)])),
                                VarPair(VarString (Var [("3", ttPC)]),VarString (Var [("4", ttPC)]))])
    -- putStrLn ("\n Flow out: " ++ (substitute (show output) substitutions))
    assertEqual "Flow ex1" expectedOutput output

testFlowEx2 :: Test
testFlowEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/cfg.lng" ex2
    let expectedOutput = (VarList[VarPair(VarString (Var [("1", ttPC)]),VarString (Var [("2", propA), ("dummy", notBDD propA)])),
                                VarPair(VarString (Var [("2", propA)]),VarString (Var [("3", propA), ("dummy", notBDD propA)])),
                                VarPair(VarString (Var [("dummy", notBDD propA)]),VarString (Var [("3", propA), ("dummy", notBDD propA)])),
                                VarPair(VarString (Var [("3", ttPC)]),VarString (Var [("4", ttPC)]))])
    putStrLn ("\n Flow out 2: " ++ (substitute (show output) substitutions))
    assertEqual "Flow ex2" expectedOutput output

testFlowEx3 :: Test
testFlowEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/cfg.lng" ex3
    let expectedOutput = (VarList[])
    putStrLn ("\n Flow out 3: " ++ (substitute (show output) substitutions))
    assertEqual "Flow ex3" expectedOutput output

varRdTestSuite :: Test
varRdTestSuite = TestList [    TestLabel "is pair" testIsPair
                        ,   TestLabel "Elem" testElem
                        ,   TestLabel "AddUnique" testAddUnique
                        -- ,   TestLabel "AddUnique same" testAddUniqueSame
                        -- ,   TestLabel "Union" testUnion
                        ,   TestLabel "is equal 1" testIsEqual1
                        ,   TestLabel "is equal 2" testIsEqual2
                        ,   TestLabel "is equal 3" testIsEqual3
                        ,   TestLabel "is equal 4" testIsEqual4
                        ,   TestLabel "Count Asgns ex1" testCountEx1
                        ,   TestLabel "Count Asgns ex2" testCountEx2
                        ,   TestLabel "Count Asgns ex3" testCountEx3
                        ,   TestLabel "Init ex1" testInitEx1
                        ,   TestLabel "Init ex2" testInitEx2
                        ,   TestLabel "Final ex1" testFinalEx1
                        ,   TestLabel "Final ex2" testFinalEx2
                        ,   TestLabel "Final ex3" testFinalEx3
                        ,   TestLabel "Flow ex1" testFlowEx1
                        ,   TestLabel "Flow ex2" testFlowEx2
                        ,   TestLabel "Flow ex3" testFlowEx3
                        ]