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

ex2_1 = VarPair (
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
        ) -- x = 1;  tt    1

ex2_2 = VarPair (
            VarString (Var [("ASGN", propA), ("SKIP", notBDD propA)]),
            VarPair (
                VarString (Var [("2", propA), ("21", notBDD propA)]),
                VarPair (
                    VarString (Var [("y", propA), ("DUMMY", notBDD propA)]),
                    VarPair(
                        VarString (Var [("INT", propA), ("DUMMY", notBDD propA)]),
                        VarString (Var [("1", propA), ("DUMMY", notBDD propA)])
                    )
                )
            )
        ) -- y = 2;  A 2 / skip; ~A 21

ex2_3 = VarPair (
            VarString (Var [("ASGN", notBDD propA), ("SKIP", propA)]),
            VarPair (
                VarString (Var [("3", notBDD propA), ("31", propA)]),
                VarPair (
                    VarString (Var [("y", notBDD propA), ("DUMMY", propA)]),
                    VarPair(
                        VarString (Var [("INT", notBDD propA), ("DUMMY", propA)]),
                        VarString (Var [("4", notBDD  propA), ("DUMMY", propA)])
                    )
                )
            )
        ) -- y = 4;  ~A 3 / skip; A 31

ex2_4 = VarPair(
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

ex2_s1 = VarPair(
            VarString (Var [("SEQ", ttPC)]), -- Should it be ~A??
            VarPair(
                ex2_3,
                ex2_4
            )
        )

ex2_s2 = VarPair(
                VarString (Var [("SEQ", ttPC)]), -- Should it be A??
                VarPair ( 
                    ex2_2,
                    ex2_s1
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
            ex2_1,
            ex2_s2
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
            VarString (Var [("1", ttPC)]), 
            VarPair (
                VarString (Var [("x", ttPC)]), 
                VarPair (
                    VarString (Var [("CONST", ttPC)]), 
                    VarString (Var [("5", ttPC)])
                )
            )
        )
    )

s02 = VarPair(
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarString (Var [("2", ttPC)]), 
            VarPair (
                VarString (Var [("y", ttPC)]), 
                VarPair(
                    VarString (Var [("CONST", ttPC)]), 
                    VarString (Var [("1", ttPC)])
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
                VarString (Var [("4", ttPC)]), 
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
            VarString (Var [("5", ttPC)]), 
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
                            VarString (Var [("1", ttPC)])
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
                                            VarString (Var [("3", ttPC)])
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

testCount :: String -> VarValor -> VarValor -> Test
testCount name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/Count-Asgns.lng" input
    -- putStrLn ("\n Count out: " ++ (substitute (show output) substitutions))
    assertEqual ("Count Asgns " ++ name) expectedOutput output

testCountEx1 :: Test
testCountEx1 = testCount "ex1" ex1 (VarInteger (Var [(4, ttPC)]))

testCountEx2 :: Test
testCountEx2 = testCount "ex2" ex2 (VarInteger (Var [(3, propA), (3, notBDD propA)]))

testCountEx2_1 :: Test
testCountEx2_1 = testCount "ex2_1" ex2_1 (VarInteger (Var [(1, ttPC)]))

testCountEx2_2 :: Test
testCountEx2_2 = testCount "ex2_2" ex2_2 (VarInteger (Var [(1, propA), (0, notBDD propA)]))

testCountEx2_3 :: Test
testCountEx2_3 = testCount "ex2_3" ex2_3 (VarInteger (Var [(1, notBDD propA), (0, propA)]))

testCountEx2_4 :: Test
testCountEx2_4 = testCount "ex2_4" ex2_4 (VarInteger (Var [(1, ttPC)]))

testCountEx2_s1 :: Test
testCountEx2_s1 = testCount "ex2_s1" ex2_s1 (VarInteger (Var [(2, notBDD propA), (1, propA)]))

testCountEx3 :: Test
testCountEx3 = testCount "PPA" exPPA (VarInteger (Var [(4, ttPC)]))

testInit :: String -> VarValor -> VarValor -> Test
testInit name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/init.lng" input
    assertEqual ("init " ++ name) expectedOutput output

testInitEx1 :: Test
testInitEx1 = testInit "Ex1" ex1 (VarString (Var [("1", ttPC)]))

testInitEx2 :: Test
testInitEx2 = testInit "Ex2" ex2 (VarString (Var [("1", ttPC)]))

testInitEx2_1 :: Test
testInitEx2_1 = testInit "Ex2_1" ex2_1 (VarString (Var [("1", ttPC)]))

testInitEx2_2 :: Test
testInitEx2_2 = testInit "Ex2_2" ex2_2 (VarString (Var [("2", propA), ("21", notBDD propA)]))

testInitEx2_3 :: Test
testInitEx2_3 = testInit "Ex2_3" ex2_3 (VarString (Var [("3", notBDD propA), ("31", propA)]))

testInitEx2_4 :: Test
testInitEx2_4 = testInit "Ex2_4" ex2_4 (VarString (Var [("4", ttPC)]))

testInitEx3 :: Test
testInitEx3 = testInit "PPA" exPPA (VarString (Var [("1", ttPC)]))

testFinal :: String -> VarValor -> VarValor -> Test
testFinal name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/final.lng" input
    -- putStrLn ("\n final expected ex3: " ++ (substitute (show expectedOutput) substitutions))
    -- putStrLn ("\n final out " ++ name ++ ": " ++ (substitute (show output) substitutions))
    assertEqual ("final " ++ name) expectedOutput output

testFinalEx1 :: Test
testFinalEx1 = testFinal "ex1" ex1 (VarList [VarString (Var [("4", ttPC)])])

testFinalEx2 :: Test
testFinalEx2 = testFinal "ex2" ex2 (VarList [VarString (Var [("4", ttPC)])])

testFinalEx2_1 :: Test
testFinalEx2_1 = testFinal "ex2_1" ex2_1 (VarList [VarString (Var [("1", ttPC)])])

testFinalEx2_2 :: Test
testFinalEx2_2 = testFinal "ex2_2" ex2_2 (VarList [VarString (Var [("2", propA), ("21", notBDD propA)])])

testFinalEx2_3 :: Test
testFinalEx2_3 = testFinal "ex2_3" ex2_3 (VarList [VarString (Var [("3", notBDD propA), ("31", propA)])])

testFinalEx2_4 :: Test
testFinalEx2_4 = testFinal "ex2_4" ex2_4 (VarList [VarString (Var [("4", ttPC)])])

testFinalEx3 :: Test
testFinalEx3 = testFinal "PPA" exPPA (VarList [VarString (Var [("3", ttPC)])])

testFlow :: String -> VarValor -> VarValor -> Test
testFlow name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/cfg.lng" input
    -- putStrLn ("\n Flow out " ++ name ++ " : " ++ (substitute (show output) substitutions))
    assertEqual ("flow " ++ name) expectedOutput output

testFlowEx1 :: Test
testFlowEx1 = testFlow "ex1" ex1 (VarList[VarPair(VarString (Var [("1", ttPC)]),VarString (Var [("2", ttPC)])),
                                VarPair(VarString (Var [("2", ttPC)]),VarString (Var [("3", ttPC)])),
                                VarPair(VarString (Var [("3", ttPC)]),VarString (Var [("4", ttPC)]))])

testFlowEx2 :: Test
testFlowEx2 = testFlow "ex2" ex2 (VarList[VarPair(VarString (Var [("1", ttPC)]),VarString (Var [("2", propA), ("21", notBDD propA)])),
                                VarPair(VarString (Var [("2", propA), ("21", notBDD propA)]), VarString (Var [("3", notBDD propA), ("31", propA)])),
                                VarPair(VarString (Var [("3", notBDD propA), ("31", propA)]), VarString (Var [("4", ttPC)]))])

testFlowEx3 :: Test
testFlowEx3 = testFlow "PPA" exPPA (VarList[VarPair(VarString (Var [("1", ttPC)]),VarString (Var [("2", ttPC)])),
                                VarPair(VarString (Var [("2", ttPC)]), VarString (Var [("3", ttPC)])),
                                VarPair(VarString (Var [("5", ttPC)]), VarString (Var [("3", ttPC)])),
                                VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("4", ttPC)])),
                                VarPair(VarString (Var [("4", ttPC)]), VarString (Var [("5", ttPC)]))
                            ])

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

testAssignments :: String -> VarValor -> VarValor -> Test
testAssignments name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/assignments.lng" input
    -- putStrLn ("\n Assignments " ++ name ++ " : " ++ (substitute (show output) substitutions))
    assertEqual ("Assignments " ++ name) expectedOutput output

testAssignmentsEx1 :: Test
testAssignmentsEx1 = testAssignments "ex1" ex1 (VarList[VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("4", ttPC)])),
                                   VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("3", ttPC)])),
                                   VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                                   VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)]))])

testAssignmentsEx2_1 :: Test
testAssignmentsEx2_1 = testAssignments "ex2_1" ex2_1 (VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)]))])

testAssignmentsEx2_2 :: Test
testAssignmentsEx2_2 = testAssignments "ex2_2" ex2_2 (VarList[VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)]))])

testAssignmentsEx2 :: Test
testAssignmentsEx2 = testAssignments "ex2" ex2 (VarList[VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("4", ttPC)])),
                                   VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("3", notBDD propA)])),
                                   VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                                   VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)]))])

testAssignmentsEx3 :: Test
testAssignmentsEx3 = testAssignments "PPA" exPPA (VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                                   VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                                   VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                                   VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)]))])

testfv :: String -> VarValor -> VarValor -> Test
testfv name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/fv.lng" input
    -- putStrLn ("\n fv " ++ name ++ " : " ++ (substitute (show output) substitutions))
    assertEqual ("fv " ++ name) expectedOutput output

testfvEx1 :: Test
testfvEx1 = testfv "ex1" ex1 (VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)]), VarString (Var [("z", ttPC)])])

testfvEx2_1 :: Test
testfvEx2_1 = testfv "ex2_1" ex2_1 (VarList[VarString (Var [("x", ttPC)])])

testfvEx2_2 :: Test
testfvEx2_2 = testfv "ex2_2" ex2_2 (VarList[VarString (Var [("y", propA)])])

testfvEx2_3 :: Test
testfvEx2_3 = testfv "ex2_3" ex2_3 (VarList[VarString (Var [("y", notBDD propA)])])

testfvEx2_4 :: Test
testfvEx2_4 = testfv "ex2_4" ex2_4 (VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)]), VarString (Var [("z", ttPC)])])

testfvEx2_s1 :: Test
testfvEx2_s1 = testfv "ex2_s1" ex2_s1 (VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)]), VarString (Var [("z", ttPC)])])

testfvEx2_s2 :: Test
testfvEx2_s2 = testfv "ex2_s2" ex2_s2 (VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)]), VarString (Var [("z", ttPC)])])

testfvEx2 :: Test
testfvEx2 = testfv "ex2" ex2 (VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)]), VarString (Var [("z", ttPC)])])

testfvEx3 :: Test
testfvEx3 = testfv "PPA" exPPA (VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)])])

testmakeSetOfFVEx1 :: Test
testmakeSetOfFVEx1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/makeSetOfFV.lng" ex1
    let expectedOutput = VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
                                   VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])), 
                                   VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]
    assertEqual "makeSetOfFV ex1" expectedOutput output

testFilterFlow :: Test
testFilterFlow = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/filterFlow.lng" (VarPair(VarString (Var [("3", ttPC)]), VarList[
            VarPair(VarString (Var [("1", ttPC)]), VarString (Var [("2", ttPC)])), 
            VarPair(VarString (Var [("2", ttPC)]), VarString (Var [("3", ttPC)])),
            VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("4", ttPC)])),
            VarPair(VarString (Var [("4", ttPC)]), VarString (Var [("5", ttPC)])),
            VarPair(VarString (Var [("5", ttPC)]), VarString (Var [("3", ttPC)])),
            VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("6", ttPC)]))]))
    let expectedOutput = (VarList[ 
            VarPair(VarString (Var [("2", ttPC)]), VarString (Var [("3", ttPC)])),
            VarPair(VarString (Var [("5", ttPC)]), VarString (Var [("3", ttPC)]))])
    assertEqual "Flow Factorial" expectedOutput output

exPPAEntry = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ])]

exPPAExit = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])) ]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ]),
    VarPair(VarString (Var [("4", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ]),
    VarPair(VarString (Var [("5", ttPC)]), VarList[
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ])]

testFindOrDefault :: Test
testFindOrDefault = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findOrDefault.lng" (
                                VarPair(
                                    VarString (Var [("2", ttPC)]), 
                                    exPPAExit))

    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])) ]
    assertEqual "testFindOrDefault PPA" expectedOutput output

testFindOrDefault2 :: Test
testFindOrDefault2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findOrDefault.lng" (
                                VarPair(
                                    VarString (Var [("6", ttPC)]), 
                                    exPPAExit))

    let expectedOutput = VarList[]
    assertEqual "testFindOrDefault2 PPA" expectedOutput output

testRDEntry1 :: Test
testRDEntry1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdEntry.lng" (
                                VarPair(VarString (Var [("1", ttPC)]), 
                                          VarPair(
                                            exPPA, 
                                            exPPAExit)))
    let expectedOutput = VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
                                   VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]
    assertEqual "rdEntry 1" expectedOutput output

testRDEntry2 :: Test
testRDEntry2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdEntry.lng" (
                                VarPair(
                                    VarString (Var [("2", ttPC)]), 
                                    VarPair(
                                        exPPA, 
                                        exPPAExit)))

    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]
    assertEqual "rdEntry 2" expectedOutput output

testRDEntry4 :: Test
testRDEntry4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdEntry.lng" (
                                VarPair(
                                    VarString (Var [("4", ttPC)]), 
                                    VarPair(
                                        exPPA, 
                                        exPPAExit)))

    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ]
    assertEqual "rdEntry 2" expectedOutput output

testfindBlock1 :: Test
testfindBlock1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findBlock.lng" (VarPair(VarString (Var [("1", ttPC)]), exPPA))
    let expectedOutput = VarList[s01]
    assertEqual "testfindBlock 1" expectedOutput output

testfindBlock2 :: Test
testfindBlock2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findBlock.lng" (VarPair(VarString (Var [("2", ttPC)]), exPPA))
    let expectedOutput = VarList[s02]
    assertEqual "testfindBlock 2" expectedOutput output

testfindBlock3 :: Test
testfindBlock3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findBlock.lng" (VarPair(VarString (Var [("3", ttPC)]), exPPA))
    let expectedOutput = VarList[whileTeste]
    assertEqual "testfindBlock 3" expectedOutput output

testfindBlock4 :: Test
testfindBlock4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findBlock.lng" (VarPair(VarString (Var [("4", ttPC)]), exPPA))
    let expectedOutput = VarList[whileS1]
    assertEqual "testfindBlock 4" expectedOutput output

testfindBlock5 :: Test
testfindBlock5 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/findBlock.lng" (VarPair(VarString (Var [("5", ttPC)]), exPPA))
    let expectedOutput = VarList[whileS2]
    assertEqual "testfindBlock 5" expectedOutput output

testGenRD1 :: Test
testGenRD1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/genRD.lng" (s01)
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])) ]
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
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])) ]
    assertEqual "test killRD" expectedOutput output

testRDExit1 :: Test
testRDExit1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdExit.lng" (
                                VarPair(VarString (Var [("1", ttPC)]), 
                                          VarPair(
                                            exPPA, 
                                            exPPAEntry)))
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]
    assertEqual "rdExit 1" expectedOutput output

testRDExit2 :: Test
testRDExit2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdExit.lng" (
                                VarPair(VarString (Var [("2", ttPC)]), 
                                          VarPair(
                                            exPPA, 
                                            exPPAEntry)))
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])), 
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])) ]
    assertEqual "rdExit 2" expectedOutput output

testRDExit4 :: Test
testRDExit4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/rdExit.lng" (
                                VarPair(VarString (Var [("4", ttPC)]), 
                                          VarPair(
                                            exPPA, 
                                            exPPAEntry)))
    let expectedOutput = VarList[
                        VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                        VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                        VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ]
    assertEqual "rdExit 4" expectedOutput output

testLabels :: Test
testLabels = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/labels.lng" (exPPA)
    let expectedOutput = VarList[VarString (Var [("1", ttPC)]), VarString (Var [("2", ttPC)]), VarString (Var [("3", ttPC)]), VarString (Var [("4", ttPC)]), VarString (Var [("5", ttPC)])]
    assertEqual "labels" expectedOutput output

testInsertInto1 :: Test
testInsertInto1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/insertIntoMap.lng" (VarPair(
                        VarString (Var [("2", ttPC)]),
                        VarPair(
                            VarList [
                                VarPair(VarString (Var [("teste", ttPC)]), VarString (Var [("teste", ttPC)])), 
                                VarPair(VarString (Var [("teste2", ttPC)]), VarString (Var [("teste2", ttPC)]))],
                            exPPAEntry
                        )))
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("1", ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
                            VarPair(VarString (Var [("2", ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
                                                    VarPair(VarString (Var [("teste", ttPC)]), VarString (Var [("teste", ttPC)])), 
                                                    VarPair(VarString (Var [("teste2", ttPC)]), VarString (Var [("teste2", ttPC)])) ]),
                            VarPair(VarString (Var [("3", ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ]),
                            VarPair(VarString (Var [("4", ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])), 
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ]),
                            VarPair(VarString (Var [("5", ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ])]
    -- putStrLn ("\n testInsertInto : " ++ (substitute (show output) substitutions))
    assertEqual "testInsertInto" expectedOutput output

testInsertInto2 :: Test
testInsertInto2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/insertIntoMap.lng" (VarPair(
                        VarString (Var [("2", ttPC)]),
                        VarPair(
                            VarList [
                                VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)]))],
                            exPPAEntry
                        )))
    let expectedOutput = exPPAEntry
    -- putStrLn ("\n testInsertInto2 : " ++ (substitute (show output) substitutions))
    assertEqual "testInsertInto" expectedOutput output

testUpdateMappings :: Test
testUpdateMappings = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/updateMappings.lng" (exPPA)
    let expectedOutput = VarPair(exPPAEntry, exPPAExit)
    assertEqual "testUpdateMappings" expectedOutput output

testReachingDefinitionsPPA :: Test
testReachingDefinitionsPPA = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/reachingDefinitions.lng" (VarPair(VarInteger (Var [(10, ttPC)]), exPPA))
    let expectedOutput = VarPair(exPPAEntry, exPPAExit)
    assertEqual "testReachingDefinitionsPPA" expectedOutput output

testUnion :: String -> VarValor -> VarValor -> Test
testUnion name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/taint/union.lng" input
    -- putStrLn ("\n union " ++ name ++ ", result: " ++ (substitute (show output) substitutions))
    assertEqual ("union " ++ name) expectedOutput output

testUnion1 :: Test
testUnion1 = testUnion "VarList {list = [VarString {str = {('a', tt)}}]} and VarList {list = [VarString {str = {('a', tt)}}]}" (VarPair(VarList [VarString (Var [("a", ttPC)])], VarList [VarString (Var [("a", ttPC)])])) (VarList [VarString (Var [("a", ttPC)])]) 

testUnion2 :: Test
testUnion2 = testUnion "VarList {list = [VarString {str = {('a', A)}}]} and VarList {list = [VarString {str = {('a', tt)}}]}" (VarPair(VarList [VarString (Var [("a", propA)])], VarList [VarString (Var [("a", ttPC)])])) (VarList [VarString (Var [("a", ttPC)])]) 

testUnion2_2 :: Test
testUnion2_2 = testUnion "VarList {list = [VarString {str = {('a', tt)}}]} and VarList {list = [VarString {str = {('a', A)}}]}" (VarPair(VarList [VarString (Var [("a", ttPC)])], VarList [VarString (Var [("a", propA)])])) (VarList [VarString (Var [("a", ttPC)])]) 

testUnion3 :: Test
testUnion3 = testUnion "VarList {list = [VarString {str = {('a', ~A)}}]} and VarList {list = [VarString {str = {('a', tt)}}]}" (VarPair(VarList [VarString (Var [("a", notBDD propA)])], VarList [VarString (Var [("a", ttPC)])])) (VarList [VarString (Var [("a", ttPC)])]) 

testUnion4 :: Test
testUnion4 = testUnion "VarList {list = [VarString {str = {('a', A)}}]} and VarList {list = [VarString {str = {('a', ~A)}}]}" (VarPair(VarList [VarString (Var [("a", propA)])], VarList [VarString (Var [("a", notBDD propA)])])) (VarList [VarString (Var [("a", propA)]), VarString (Var [("a", notBDD propA)])]) 

testUnion5 :: Test
testUnion5 = testUnion "VarList {list = [VarString {str = {('a', tt)}}]} and VarList {list = [VarString {str = {('a', ff)}}]}" (VarPair(VarList [VarString (Var [("a", ttPC)])], VarList [VarString (Var [("a", ffPC)])])) (VarList [VarString (Var [("a", ttPC)]), VarString (Var [("a", ffPC)])]) 

testUnion6 :: Test
testUnion6 = testUnion "VarList {list = [VarString {str = {('a', A)}}]} and VarList {list = [VarString {str = {('a', ff)}}]}" (VarPair(VarList [VarString (Var [("a", propA)])], VarList [VarString (Var [("a", ffPC)])])) (VarList [VarString (Var [("a", propA)]), VarString (Var [("a", ffPC)])]) 

varRdTestSuite :: Test
varRdTestSuite = TestList [    TestLabel "is pair" testIsPair
                        ,   TestLabel "Elem" testElem
                        ,   TestLabel "is equal 1" testIsEqual1
                        ,   TestLabel "is equal 2" testIsEqual2
                        ,   TestLabel "is equal 3" testIsEqual3
                        ,   TestLabel "is equal 4" testIsEqual4
                        ,   TestLabel "Count Asgns ex1" testCountEx1
                        ,   TestLabel "Count Asgns ex2" testCountEx2
                        ,   TestLabel "Count Asgns ex2_1" testCountEx2_1
                        ,   TestLabel "Count Asgns ex2_2" testCountEx2_2
                        ,   TestLabel "Count Asgns ex2_3" testCountEx2_3
                        ,   TestLabel "Count Asgns ex2_4" testCountEx2_4
                        ,   TestLabel "Count Asgns ex2_s1" testCountEx2_s1
                        ,   TestLabel "Count Asgns ex3" testCountEx3
                        ,   TestLabel "Init Ex2_1" testInitEx2_1
                        ,   TestLabel "Init Ex2_2" testInitEx2_2
                        ,   TestLabel "Init Ex2_3" testInitEx2_3
                        ,   TestLabel "Init Ex2_4" testInitEx2_4
                        ,   TestLabel "Init ex1" testInitEx1
                        ,   TestLabel "Init ex2" testInitEx2
                        ,   TestLabel "Init ex3" testInitEx3
                        ,   TestLabel "Final ex1" testFinalEx1
                        ,   TestLabel "Final ex2" testFinalEx2
                        ,   TestLabel "Final ex2_1" testFinalEx2_1
                        ,   TestLabel "Final ex2_2" testFinalEx2_2
                        ,   TestLabel "Final ex2_3" testFinalEx2_3
                        ,   TestLabel "Final ex2_4" testFinalEx2_4
                        ,   TestLabel "Final ex3" testFinalEx3
                        ,   TestLabel "Flow ex1" testFlowEx1
                        ,   TestLabel "Flow ex2" testFlowEx2
                        ,   TestLabel "Flow ex3" testFlowEx3
                        ,   TestLabel "Chaotic Iteration 1" testChaoticIteration1
                        ,   TestLabel "Chaotic Iteration 2" testChaoticIteration2
                        ,   TestLabel "Asgns ex1" testAssignmentsEx1
                        ,   TestLabel "Asgns ex2_1" testAssignmentsEx2_1
                        ,   TestLabel "Asgns ex2_2" testAssignmentsEx2_2
                        ,   TestLabel "Asgns ex2" testAssignmentsEx2
                        ,   TestLabel "Asgns ex3" testAssignmentsEx3
                        ,   TestLabel "fv ex1" testfvEx1
                        ,   TestLabel "fv ex2_1" testfvEx2_1
                        ,   TestLabel "fv ex2_2" testfvEx2_2
                        ,   TestLabel "fv ex2_3" testfvEx2_3
                        ,   TestLabel "fv ex2_4" testfvEx2_4
                        ,   TestLabel "fv ex2_s1" testfvEx2_s1
                        ,   TestLabel "fv ex2_s2" testfvEx2_s2
                        ,   TestLabel "fv ex3" testfvEx3
                        ,   TestLabel "testmakeSetOfFVEx1" testmakeSetOfFVEx1
                        ,   TestLabel "testFilterFlow" testFilterFlow
                        ,   TestLabel "testFindOrDefault" testFindOrDefault
                        ,   TestLabel "testFindOrDefault2" testFindOrDefault2
                        ,   TestLabel "rdEntry 1" testRDEntry1
                        ,   TestLabel "rdEntry 2" testRDEntry2
                        ,   TestLabel "rdEntry 4" testRDEntry4
                        ,   TestLabel "testfindBlock 1" testfindBlock1
                        ,   TestLabel "testfindBlock 2" testfindBlock2
                        ,   TestLabel "testfindBlock 3" testfindBlock3
                        ,   TestLabel "testfindBlock 4" testfindBlock4
                        ,   TestLabel "testfindBlock 5" testfindBlock5
                        ,   TestLabel "testGenRD 1" testGenRD1
                        ,   TestLabel "testGenRD 2" testGenRD2
                        ,   TestLabel "testKillRD" testKillRD
                        ,   TestLabel "rdExit 1" testRDExit1
                        ,   TestLabel "rdExit 2" testRDExit2
                        ,   TestLabel "rdExit 4" testRDExit4
                        ,   TestLabel "labels" testLabels
                        ,   TestLabel "testInsertInto 1" testInsertInto1
                        ,   TestLabel "testInsertInto 2" testInsertInto2
                        ,   TestLabel "testReachingDefinitionsPPA" testReachingDefinitionsPPA
                        ,   TestLabel "union 1" testUnion1
                        ,   TestLabel "union 2" testUnion2
                        ,   TestLabel "union 2_2" testUnion2_2
                        ,   TestLabel "union 3" testUnion3
                        ,   TestLabel "union 4" testUnion4
                        ,   TestLabel "union 5" testUnion5
                        ,   TestLabel "union 6" testUnion6
                        ]