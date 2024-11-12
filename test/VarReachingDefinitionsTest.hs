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

-- s01 = Assignment "x" (Const 5) 1
-- s02 = Assignment "y" (Const 1) 2

-- whileTeste = (GTExp (Var "x") (Const 1), 3)
-- whileS1 = Assignment "y" (Mult (Var "x") (Var "y")) 4
-- whileS2 = Assignment "x" (Sub (Var "x") (Const 1)) 5
-- s03 = While whileTeste (Seq whileS1 whileS2)

s01 = VarPair(
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarInteger (Var [(1, ttPC)]), 
            VarPair (
                VarString (Var [("x", ttPC)]), 
                VarPair (
                    VarString (Var [("CONST", ttPC)]), 
                    VarInteger (Var [(5, ttPC)])
                )
            )
        )
    )

s02 = VarPair(
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarInteger (Var [(2, ttPC)]), 
            VarPair (
                VarString (Var [("y", ttPC)]), 
                VarPair(
                    VarString (Var [("CONST", ttPC)]), 
                    VarInteger (Var [(1, ttPC)])
                )
            )
        )
    )

whileTeste = VarPair (
                VarString (Var [("VAR", ttPC)]), 
                VarString (Var [("x", ttPC)])
            )

whileS1 = VarPair(
            VarString (Var [("ASGN", ttPC)]),
            VarPair (
                VarInteger (Var [(4, ttPC)]), 
                VarPair (
                    VarString (Var [("y", ttPC)]), 
                    VarPair (
                        VarString (Var [("MULT", ttPC)]), 
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
        )

whileS2 = VarPair(
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarInteger (Var [(5, ttPC)]), 
            VarPair (
                VarString (Var [("x", ttPC)]), 
                VarPair (
                    VarString (Var [("SUB", ttPC)]), 
                    VarPair (
                        VarPair (
                            VarString (Var [("VAR", ttPC)]),
                            VarString (Var [("x", ttPC)])
                        ),
                        VarPair(
                            VarString (Var [("CONST", ttPC)]), 
                            VarInteger (Var [(1, ttPC)])
                        )
                    )
                )
            )
        )
    )

exPPA =  VarPair (
                    VarString (Var [("SEQ", ttPC)]),
                    VarPair(
                        s01,
                        VarPair(
                            VarString (Var [("SEQ", ttPC)]),
                            VarPair (
                                s02,
                                VarPair (
                                    VarString (Var [("WHILE", ttPC)]), 
                                    VarPair ( 
                                        VarPair(
                                            whileTeste,
                                            VarInteger (Var [(3, ttPC)])
                                        ),
                                        VarPair (
                                            VarString (Var [("SEQ", ttPC)]),
                                            VarPair(
                                                whileS1,
                                                whileS2
                                            )
                                        )
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
--     output <- processFile executeProg "src/Language/Examples/taint/addUnique.lng" (VarPair(
--             ValorStr "b",
--             VarList [ValorBool True, ValorStr "b", VarInteger (Var [(4, ttPC)])]
--         ))
--     let expectedOutput = (VarList [ValorBool True, ValorStr "b", VarInteger (Var [(4, ttPC)])])
--     assertEqual "addUnique same" expectedOutput output

-- testUnion :: Test
-- testUnion = TestCase $ do
--     output <- processFile executeProg "src/Language/Examples/taint/union.lng" (VarPair(
--             VarList [ValorStr "a", ValorStr "b", VarInteger (Var [(1, ttPC)])],
--             VarList [ValorBool True, ValorStr "b", VarInteger (Var [(4, ttPC)])]
--         ))
--     let expectedOutput = (VarList [ValorStr "a", ValorStr "b", VarInteger (Var [(1, ttPC)]), ValorBool True, VarInteger (Var [(4, ttPC)])])
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

testChaoticIteration1 :: Test
testChaoticIteration1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/chaoticIteration.lng" (VarPair(VarInteger (Var [(1, ttPC)]), VarInteger (Var [(2, ttPC)])))
    let expectedOutput = (VarInteger (Var [(3, ttPC)]))
    assertEqual "chaotic iteration 1 test" expectedOutput output

testChaoticIteration2 :: Test
testChaoticIteration2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/chaoticIteration.lng" (VarPair(VarInteger (Var [(2, ttPC)]), VarInteger (Var [(2, ttPC)])))
    let expectedOutput = (VarInteger (Var [(4, ttPC)]))
    assertEqual "chaotic iteration 2 test" expectedOutput output

testAssignmentsEx1 :: Test
testAssignmentsEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/assignments.lng" ex1
    let expectedOutput = VarList[VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])),
                                   VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(2, ttPC)])),
                                   VarPair(VarString (Var [("z", ttPC)]), VarInteger (Var [(3, ttPC)]))]
    assertEqual " Asgns ex1" expectedOutput output

testAssignmentsEx2 :: Test
testAssignmentsEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/assignments.lng" ex2
    let expectedOutput = VarList[VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)]))]
    assertEqual " Asgns ex2" expectedOutput output


testAssignmentsEx3 :: Test
testAssignmentsEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/assignments.lng" ex3
    let expectedOutput = VarList[VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])),
                                   VarPair(VarString (Var [("soma", ttPC)]), VarInteger (Var [(2, ttPC)])),
                                   VarPair(VarString (Var [("c", ttPC)]), VarInteger (Var [(3, ttPC)])),
                                   VarPair(VarString (Var [("soma", ttPC)]), VarInteger (Var [(5, ttPC)])),
                                   VarPair(VarString (Var [("c", ttPC)]), VarInteger (Var [(6, ttPC)]))]
    assertEqual " Asgns ex3" expectedOutput output


testfvEx1 :: Test
testfvEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/fv.lng" ex1
    let expectedOutput = VarList[VarString (Var [("z", ttPC)]), VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)])]
    assertEqual "fv ex1" expectedOutput output

testfvEx2 :: Test
testfvEx2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/fv.lng" ex2
    let expectedOutput = VarList[VarString (Var [("x", ttPC)])]
    assertEqual "fv ex2" expectedOutput output


testfvEx3 :: Test
testfvEx3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/fv.lng" ex3
    let expectedOutput = VarList[VarString (Var [("x", ttPC)]), VarString (Var [("soma", ttPC)]), VarString (Var [("c", ttPC)])]
    assertEqual "fv ex3" expectedOutput output

testmakeSetOfFVEx1 :: Test
testmakeSetOfFVEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/makeSetOfFV.lng" ex1
    let expectedOutput = VarList[VarPair(VarString (Var [("z", ttPC)]), VarInteger (Var [(-1, ttPC)])), 
                                   VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(-1, ttPC)])), 
                                   VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(-1, ttPC)])) ]
    assertEqual "makeSetOfFV ex1" expectedOutput output

testFilterFlow :: Test
testFilterFlow = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/filterFlow.lng" (VarPair(VarInteger (Var [(3, ttPC)]), VarList[
            VarPair(VarInteger (Var [(1, ttPC)]), VarInteger (Var [(2, ttPC)])), 
            VarPair(VarInteger (Var [(2, ttPC)]), VarInteger (Var [(3, ttPC)])),
            VarPair(VarInteger (Var [(3, ttPC)]), VarInteger (Var [(4, ttPC)])),
            VarPair(VarInteger (Var [(4, ttPC)]), VarInteger (Var [(5, ttPC)])),
            VarPair(VarInteger (Var [(5, ttPC)]), VarInteger (Var [(3, ttPC)])),
            VarPair(VarInteger (Var [(3, ttPC)]), VarInteger (Var [(6, ttPC)]))]))
    let expectedOutput = (VarList[ 
            VarPair(VarInteger (Var [(2, ttPC)]), VarInteger (Var [(3, ttPC)])),
            VarPair(VarInteger (Var [(5, ttPC)]), VarInteger (Var [(3, ttPC)]))])
    assertEqual "Flow Factorial" expectedOutput output

exPPAEntry = VarList[
    VarPair(VarInteger (Var [(1, ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(-1, ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(-1, ttPC)])) ]),
    VarPair(VarInteger (Var [(2, ttPC)]), VarList[
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(-1, ttPC)])), 
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])) ]),
    VarPair(VarInteger (Var [(3, ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(2, ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ]),
    VarPair(VarInteger (Var [(4, ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(2, ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ]),
    VarPair(VarInteger (Var [(5, ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ])]

exPPAExit = VarList[
    VarPair(VarInteger (Var [(1, ttPC)]), VarList[
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(-1, ttPC)])), 
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])) ]),
    VarPair(VarInteger (Var [(2, ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(2, ttPC)])) ]),
    VarPair(VarInteger (Var [(3, ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(2, ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ]),
    VarPair(VarInteger (Var [(4, ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ]),
    VarPair(VarInteger (Var [(5, ttPC)]), VarList[
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ])]

testFindOrDefault :: Test
testFindOrDefault = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findOrDefault.lng" (
                                VarPair(
                                    VarInteger (Var [(2, ttPC)]), 
                                    exPPAExit))

    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(2, ttPC)])) ]
    assertEqual "testFindOrDefault PPA" expectedOutput output

testFindOrDefault2 :: Test
testFindOrDefault2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findOrDefault.lng" (
                                VarPair(
                                    VarInteger (Var [(6, ttPC)]), 
                                    exPPAExit))

    let expectedOutput = VarList[]
    assertEqual "testFindOrDefault2 PPA" expectedOutput output

testRDEntry1 :: Test
testRDEntry1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdEntry.lng" (
                                VarPair(VarInteger (Var [(1, ttPC)]), 
                                          VarPair(
                                            exPPA, 
                                            exPPAExit)))
    let expectedOutput = VarList[VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(-1, ttPC)])), 
                                   VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(-1, ttPC)])) ]
    assertEqual "rdEntry 1" expectedOutput output

testRDEntry2 :: Test
testRDEntry2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdEntry.lng" (
                                VarPair(
                                    VarInteger (Var [(2, ttPC)]), 
                                    VarPair(
                                        exPPA, 
                                        exPPAExit)))

    let expectedOutput = VarList[
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(-1, ttPC)])), 
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])) ]
    assertEqual "rdEntry 2" expectedOutput output

testRDEntry4 :: Test
testRDEntry4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdEntry.lng" (
                                VarPair(
                                    VarInteger (Var [(4, ttPC)]), 
                                    VarPair(
                                        exPPA, 
                                        exPPAExit)))

    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(2, ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ]
    assertEqual "rdEntry 2" expectedOutput output

testfindBlock1 :: Test
testfindBlock1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findBlock.lng" (VarPair(VarInteger (Var [(1, ttPC)]), exPPA))
    let expectedOutput = VarList[s01]
    assertEqual "testfindBlock 1" expectedOutput output

testfindBlock2 :: Test
testfindBlock2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findBlock.lng" (VarPair(VarInteger (Var [(2, ttPC)]), exPPA))
    let expectedOutput = VarList[s02]
    assertEqual "testfindBlock 2" expectedOutput output

testfindBlock3 :: Test
testfindBlock3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findBlock.lng" (VarPair(VarInteger (Var [(3, ttPC)]), exPPA))
    let expectedOutput = VarList[whileTeste]
    assertEqual "testfindBlock 3" expectedOutput output

testfindBlock4 :: Test
testfindBlock4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findBlock.lng" (VarPair(VarInteger (Var [(4, ttPC)]), exPPA))
    let expectedOutput = VarList[whileS1]
    assertEqual "testfindBlock 4" expectedOutput output

testfindBlock5 :: Test
testfindBlock5 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findBlock.lng" (VarPair(VarInteger (Var [(5, ttPC)]), exPPA))
    let expectedOutput = VarList[whileS2]
    assertEqual "testfindBlock 5" expectedOutput output

testGenRD1 :: Test
testGenRD1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/genRD.lng" (s01)
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])) ]
    assertEqual "test genRD 1" expectedOutput output

testGenRD2 :: Test
testGenRD2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/genRD.lng" (exPPA)
    let expectedOutput = VarList[]
    assertEqual "test genRD factorial" expectedOutput output

testKillRD :: Test
testKillRD = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/killRD.lng" (VarPair(s01, exPPA))
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(-1, ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ]
    assertEqual "test killRD" expectedOutput output

testRDExit1 :: Test
testRDExit1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdExit.lng" (
                                VarPair(VarInteger (Var [(1, ttPC)]), 
                                          VarPair(
                                            exPPA, 
                                            exPPAEntry)))
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(-1, ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])) ]
    assertEqual "rdExit 1" expectedOutput output

testRDExit2 :: Test
testRDExit2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdExit.lng" (
                                VarPair(VarInteger (Var [(2, ttPC)]), 
                                          VarPair(
                                            exPPA, 
                                            exPPAEntry)))
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(2, ttPC)])) ]
    assertEqual "rdExit 2" expectedOutput output

testRDExit4 :: Test
testRDExit4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdExit.lng" (
                                VarPair(VarInteger (Var [(4, ttPC)]), 
                                          VarPair(
                                            exPPA, 
                                            exPPAEntry)))
    let expectedOutput = VarList[
                        VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])),
                        VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])), 
                        VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])) ]
    assertEqual "rdExit 4" expectedOutput output

testLabels :: Test
testLabels = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/labels.lng" (exPPA)
    let expectedOutput = VarList[VarInteger (Var [(1, ttPC)]), VarInteger (Var [(2, ttPC)]), VarInteger (Var [(3, ttPC)]), VarInteger (Var [(4, ttPC)]), VarInteger (Var [(5, ttPC)])]
    assertEqual "labels" expectedOutput output

testInsertInto1 :: Test
testInsertInto1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/insertIntoMap.lng" (VarPair(
                        VarInteger (Var [(2, ttPC)]),
                        VarPair(
                            VarList [
                                VarPair(VarString (Var [("teste", ttPC)]), VarString (Var [("teste", ttPC)])), 
                                VarPair(VarString (Var [("teste2", ttPC)]), VarString (Var [("teste2", ttPC)]))],
                            exPPAEntry
                        )))
    let expectedOutput = VarList[
                            VarPair(VarInteger (Var [(1, ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(-1, ttPC)])), 
                                                    VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(-1, ttPC)])) ]),
                            VarPair(VarInteger (Var [(2, ttPC)]), VarList[
                                                    VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(-1, ttPC)])), 
                                                    VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])),
                                                    VarPair(VarString (Var [("teste", ttPC)]), VarString (Var [("teste", ttPC)])), 
                                                    VarPair(VarString (Var [("teste2", ttPC)]), VarString (Var [("teste2", ttPC)])) ]),
                            VarPair(VarInteger (Var [(3, ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])), 
                                                    VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(2, ttPC)])),
                                                    VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])),
                                                    VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ]),
                            VarPair(VarInteger (Var [(4, ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])),
                                                    VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(2, ttPC)])), 
                                                    VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])),
                                                    VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ]),
                            VarPair(VarInteger (Var [(5, ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(1, ttPC)])),
                                                    VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(4, ttPC)])),
                                                    VarPair(VarString (Var [("x", ttPC)]), VarInteger (Var [(5, ttPC)])) ])]
    assertEqual "testInsertInto" expectedOutput output

testInsertInto2 :: Test
testInsertInto2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/insertIntoMap.lng" (VarPair(
                        VarInteger (Var [(2, ttPC)]),
                        VarPair(
                            VarList [
                                VarPair(VarString (Var [("y", ttPC)]), VarInteger (Var [(-1, ttPC)]))],
                            exPPAEntry
                        )))
    let expectedOutput = exPPAEntry
    assertEqual "testInsertInto" expectedOutput output

testUpdateMappings :: Test
testUpdateMappings = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/updateMappings.lng" (exPPA)
    let expectedOutput = VarPair(exPPAEntry, exPPAExit)
    assertEqual "testUpdateMappings" expectedOutput output

testReachingDefinitionsPPA :: Test
testReachingDefinitionsPPA = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/reachingDefinitions.lng" (VarPair(VarInteger (Var [(1, ttPC)]), exPPA))
    let expectedOutput = VarPair(exPPAEntry, exPPAExit)
    assertEqual "testReachingDefinitionsPPA" expectedOutput output

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