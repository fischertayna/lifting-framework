module VarDFATest where

import Language.VInterpreter.Interpreter
import Language.VInterpreter.Driver
import Variability.VarTypes (Prop, VarValor(..), Var (Var), apply, mkBDDVar, notBDD, ttPC, ffPC, tt, ff, (/\), (\/), (|||), propA, propB, atbt, atbf, afbt, afbf, substitute)

import Language.Frontend.ErrM
import Helper (processFile)

import Test.HUnit
import System.Timeout (timeout)
import Control.Exception (evaluate)
import WhileExamples (rdS01, rdS02, rdWhileS1,rdWhileS2, rdExample, ex2While, lvExample, aeExample, vbExample)
import WhileLang.WhileEncoder (encodeStmt)
import VarExamples (
    ex1, 
    ex2_1, ex2_2, ex2_3, ex2_4, ex2Entry, ex2Exit, 
    rdExampleEntry, rdExampleExit,
    lvExampleEntry, lvExampleExit,
    a_plus_b, a_mult_b, aeExampleEntry, aeExampleExit,
    a_minus_b, b_minus_a, vbExampleEntry, vbExampleExit)

ex2 = encodeStmt ex2While

exRD = encodeStmt rdExample

exLV = encodeStmt lvExample

exAE = encodeStmt aeExample

exVB = encodeStmt vbExample

testElem :: Test
testElem = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/elem.lng" (VarPair (
                                                        VarInteger (Var [(1, propA), (2, notBDD propA)]),
                                                        VarList [VarBool (Var [(True, ttPC)]), VarString (Var [("2", ttPC)]), VarInteger (Var [(1, ttPC)])]
                                                    ))
    let expectedOutput = (VarInteger (Var [(1, propA), (0, notBDD propA)]))
    -- putStrLn ("\n elem: " ++ (substitute (show output)))
    assertEqual "elem" expectedOutput output

testIsPair :: Test
testIsPair = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/isPair.lng" ex1
    let expectedOutput = (VarBool (Var [(True, ttPC)]))
    assertEqual "is ex1 pair" expectedOutput output

testIsEqual1 :: Test
testIsEqual1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/isEqual.lng" (VarPair (
                                                        VarInteger (Var [(1, ttPC)]),
                                                        VarInteger (Var [(1, ttPC)])
                                                    ))
    let expectedOutput = (VarBool (Var [(True, ttPC)]))
    assertEqual "is ex1 pair" expectedOutput output

testIsEqual2 :: Test
testIsEqual2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/isEqual.lng" (VarPair (
                                                        VarInteger (Var [(2, propA), (1, notBDD propA)]),
                                                        VarInteger (Var [(1, ttPC)])
                                                    ))
    let expectedOutput = (VarBool (Var [(False, propA), (True, notBDD propA)]))
    assertEqual "is ex1 pair" expectedOutput output

testIsEqual3 :: Test
testIsEqual3 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/isEqual.lng" (VarPair (
                                                        VarInteger (Var [(1, propA), (2, notBDD propA)]),
                                                        VarInteger (Var [(1, propA), (3, notBDD propA)])
                                                    ))
    let expectedOutput = (VarBool (Var [(True, propA), (False, notBDD propA)]))
    assertEqual "is ex1 pair" expectedOutput output

testIsEqual4 :: Test
testIsEqual4 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/isEqual.lng" (VarPair (
                                                        VarString (Var [("ASGN", propA), ("DUMMY", notBDD propA)]),
                                                        VarString (Var [("ASGN", ttPC)])
                                                    ))
    let expectedOutput = (VarBool (Var [(True, propA), (False, notBDD propA)]))
    assertEqual "is ex1 pair" expectedOutput output

testCount :: String -> VarValor -> VarValor -> Test
testCount name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/Count-Asgns.lng" input
    -- putStrLn ("\n Count out: " ++ (substitute (show output)))
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

testCountexRD :: Test
testCountexRD = testCount "PPA" exRD (VarInteger (Var [(4, ttPC)]))

testInit :: String -> VarValor -> VarValor -> Test
testInit name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/init.lng" input
    assertEqual ("init " ++ name) expectedOutput output

testInitEx1 :: Test
testInitEx1 = testInit "Ex1" ex1 (VarString (Var [("1", ttPC)]))

testInitEx2 :: Test
testInitEx2 = testInit "Ex2" ex2 (VarString (Var [("1", ttPC)]))

testInitEx2_1 :: Test
testInitEx2_1 = testInit "Ex2_1" ex2_1 (VarString (Var [("1", ttPC)]))

testInitEx2_2 :: Test
testInitEx2_2 = testInit "Ex2_2" ex2_2 (VarString (Var [("2", propA), ("-2", notBDD propA)]))

testInitEx2_3 :: Test
testInitEx2_3 = testInit "Ex2_3" ex2_3 (VarString (Var [("3", notBDD propA), ("-3", propA)]))

testInitEx2_4 :: Test
testInitEx2_4 = testInit "Ex2_4" ex2_4 (VarString (Var [("4", ttPC)]))

testInitexRD :: Test
testInitexRD = testInit "PPA" exRD (VarString (Var [("1", ttPC)]))

testFinal :: String -> VarValor -> VarValor -> Test
testFinal name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/final.lng" input
    -- putStrLn ("\n final out " ++ name ++ ": " ++ (substitute (show output)))
    assertEqual ("final " ++ name) expectedOutput output

testFinalEx1 :: Test
testFinalEx1 = testFinal "ex1" ex1 (VarList [VarString (Var [("4", ttPC)])])

testFinalEx2 :: Test
testFinalEx2 = testFinal "ex2" ex2 (VarList [VarString (Var [("4", ttPC)])])

testFinalEx2_1 :: Test
testFinalEx2_1 = testFinal "ex2_1" ex2_1 (VarList [VarString (Var [("1", ttPC)])])

testFinalEx2_2 :: Test
testFinalEx2_2 = testFinal "ex2_2" ex2_2 (VarList [VarString (Var [("2", propA), ("-2", notBDD propA)])])

testFinalEx2_3 :: Test
testFinalEx2_3 = testFinal "ex2_3" ex2_3 (VarList [VarString (Var [("3", notBDD propA), ("-3", propA)])])

testFinalEx2_4 :: Test
testFinalEx2_4 = testFinal "ex2_4" ex2_4 (VarList [VarString (Var [("4", ttPC)])])

testFinalexRD :: Test
testFinalexRD = testFinal "PPA" exRD (VarList [VarString (Var [("3", ttPC)])])

testFlow :: String -> VarValor -> VarValor -> Test
testFlow name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/flow.lng" input
    -- putStrLn ("\n Flow out " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("flow " ++ name) expectedOutput output

testFlowEx1 :: Test
testFlowEx1 = testFlow "ex1" ex1 (VarList[VarPair(VarString (Var [("1", ttPC)]),VarString (Var [("2", ttPC)])),
                                VarPair(VarString (Var [("2", ttPC)]),VarString (Var [("3", ttPC)])),
                                VarPair(VarString (Var [("3", ttPC)]),VarString (Var [("4", ttPC)]))])

flow2 = VarList[VarPair(VarString (Var [("1", ttPC)]),VarString (Var [("2", propA), ("-2", notBDD propA)])),
                VarPair(VarString (Var [("3", notBDD propA), ("-3", propA)]), VarString (Var [("4", ttPC)])),
                VarPair(VarString (Var [("2", propA), ("-2", notBDD propA)]), VarString (Var [("3", notBDD propA), ("-3", propA)]))]

testFlowEx2 :: Test
testFlowEx2 = testFlow "ex2" ex2 (flow2)

testFlowexRD :: Test
testFlowexRD = testFlow "PPA" exRD (VarList[VarPair(VarString (Var [("1", ttPC)]),VarString (Var [("2", ttPC)])),
                                VarPair(VarString (Var [("2", ttPC)]), VarString (Var [("3", ttPC)])),
                                VarPair(VarString (Var [("5", ttPC)]), VarString (Var [("3", ttPC)])),
                                VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("4", ttPC)])),
                                VarPair(VarString (Var [("4", ttPC)]), VarString (Var [("5", ttPC)]))
                            ])

testFlowR :: String -> VarValor -> VarValor -> Test
testFlowR name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/flowR.lng" input
    -- putStrLn ("\n FlowR out " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("flowR " ++ name) expectedOutput output

testFlowREx1 :: Test
testFlowREx1 = testFlowR "ex1" ex1 (VarList[VarPair(VarString (Var [("2", ttPC)]),VarString (Var [("1", ttPC)])),
                                VarPair(VarString (Var [("3", ttPC)]),VarString (Var [("2", ttPC)])),
                                VarPair(VarString (Var [("4", ttPC)]),VarString (Var [("3", ttPC)]))])

flow2R = VarList[VarPair(VarString (Var [("2", propA), ("-2", notBDD propA)]), VarString (Var [("1", ttPC)])),
                VarPair(VarString (Var [("4", ttPC)]), VarString (Var [("3", notBDD propA), ("-3", propA)])),
                VarPair(VarString (Var [("3", notBDD propA), ("-3", propA)]), VarString (Var [("2", propA), ("-2", notBDD propA)]))]

testFlowREx2 :: Test
testFlowREx2 = testFlowR "ex2" ex2 (flow2R)

testFlowRexRD :: Test
testFlowRexRD = testFlowR "PPA" exRD (VarList[VarPair(VarString (Var [("2", ttPC)]), VarString (Var [("1", ttPC)])),
                                VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("2", ttPC)])),
                                VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("5", ttPC)])),
                                VarPair(VarString (Var [("4", ttPC)]), VarString (Var [("3", ttPC)])),
                                VarPair(VarString (Var [("5", ttPC)]), VarString (Var [("4", ttPC)]))
                            ])

testChaoticIteration1 :: Test
testChaoticIteration1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/chaoticIteration.lng" (VarPair(VarInteger (Var [(1, ttPC)]), VarInteger (Var [(2, ttPC)])))
    let expectedOutput = (VarInteger (Var [(3, ttPC)]))
    assertEqual "chaotic iteration 1 test" expectedOutput output

testChaoticIteration2 :: Test
testChaoticIteration2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/chaoticIteration.lng" (VarPair(VarInteger (Var [(2, ttPC)]), VarInteger (Var [(2, ttPC)])))
    let expectedOutput = (VarInteger (Var [(4, ttPC)]))
    assertEqual "chaotic iteration 2 test" expectedOutput output

testAssignments :: String -> VarValor -> VarValor -> Test
testAssignments name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/assignments.lng" input
    -- putStrLn ("\n Assignments " ++ name ++ " : " ++ (substitute (show output)))
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

testAssignmentsexRD :: Test
testAssignmentsexRD = testAssignments "PPA" exRD (VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                                   VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                                   VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                                   VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)]))])

testfv :: String -> VarValor -> VarValor -> Test
testfv name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/fv.lng" input
    -- putStrLn ("\n fv " ++ name ++ " : " ++ (substitute (show output)))
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

testfvEx2 :: Test
testfvEx2 = testfv "ex2" ex2 (VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)]), VarString (Var [("z", ttPC)])])

testfvexRD :: Test
testfvexRD = testfv "PPA" exRD (VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)])])

testmakeSetOfFV :: String -> VarValor -> VarValor -> Test
testmakeSetOfFV name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/makeSetOfFV.lng" input
    -- putStrLn ("\n makeSetOfFV " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("makeSetOfFV " ++ name) expectedOutput output

testmakeSetOfFVEx1 :: Test
testmakeSetOfFVEx1 = testmakeSetOfFV "ex1" ex1 (VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
                                   VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])), 
                                   VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testmakeSetOfFVEx2 :: Test
testmakeSetOfFVEx2 = testmakeSetOfFV "ex2" ex2 (VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
                                   VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])), 
                                   VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testFilterFlowBase :: String -> VarValor -> VarValor -> Test
testFilterFlowBase name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/filterFlow.lng" input
    -- putStrLn ("\n FilterFlow " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("FilterFlow " ++ name) expectedOutput output

testFilterFlow :: Test
testFilterFlow = testFilterFlowBase "ex factorial" (VarPair(VarString (Var [("3", ttPC)]), VarList[
            VarPair(VarString (Var [("1", ttPC)]), VarString (Var [("2", ttPC)])), 
            VarPair(VarString (Var [("2", ttPC)]), VarString (Var [("3", ttPC)])),
            VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("4", ttPC)])),
            VarPair(VarString (Var [("4", ttPC)]), VarString (Var [("5", ttPC)])),
            VarPair(VarString (Var [("5", ttPC)]), VarString (Var [("3", ttPC)])),
            VarPair(VarString (Var [("3", ttPC)]), VarString (Var [("6", ttPC)]))])) (VarList[ 
            VarPair(VarString (Var [("2", ttPC)]), VarString (Var [("3", ttPC)])),
            VarPair(VarString (Var [("5", ttPC)]), VarString (Var [("3", ttPC)]))])

testFilterFlowEx2_2 :: Test
testFilterFlowEx2_2 = testFilterFlowBase "ex 2: 2" (VarPair(VarString (Var [("2", ttPC)]), flow2)) (VarList[ 
            VarPair(VarString (Var [("1", propA)]), VarString (Var [("2", propA)])) ])

testFilterFlowEx2_3 :: Test
testFilterFlowEx2_3 = testFilterFlowBase "ex 2: 3" (VarPair(VarString (Var [("3", ttPC)]), flow2)) (VarList[ 
            VarPair(VarString (Var [("-2", notBDD propA)]), VarString (Var [("3", notBDD propA)])) ])

testFilterFlowEx2_31 :: Test
testFilterFlowEx2_31 = testFilterFlowBase "ex 2: 31" (VarPair(VarString (Var [("-3", ttPC)]), flow2)) (VarList[ 
            VarPair(VarString (Var [("2", propA)]), VarString (Var [("-3", propA)])) ])

testFilterFlowEx2_4 :: Test
testFilterFlowEx2_4 = testFilterFlowBase "ex 2: 4" (VarPair(VarString (Var [("4", ttPC)]), flow2)) (VarList[ 
            VarPair(VarString (Var [("3", notBDD propA), ("-3", propA)]), VarString (Var [("4", ttPC)])) ])

testRDEntry :: String -> VarValor -> VarValor -> Test
testRDEntry name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/rdEntry.lng" input
    -- putStrLn ("\n RDEntry " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("RDEntry " ++ name) (show expectedOutput) (show output)

testRDEntry2_1 :: Test
testRDEntry2_1 = testRDEntry "Ex2_1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(ex2, ex2Exit)))  (
    VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testRDEntry2_2 :: Test
testRDEntry2_2 = testRDEntry "Ex2_2" (VarPair(VarString (Var [("2", propA), ("-2", notBDD propA)]), VarPair(ex2, ex2Exit)))  (
    VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testRDEntry2_3 :: Test
testRDEntry2_3 = testRDEntry "Ex2_3" (VarPair(VarString (Var [("3", notBDD propA), ("-3", propA)]), VarPair(ex2, ex2Exit)))  (
    VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                    VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                    VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("?", notBDD propA)])),
                    VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testRDEntry2_4 :: Test
testRDEntry2_4 = testRDEntry "Ex2_4" (VarPair(VarString (Var [("4", ttPC)]), VarPair(ex2, ex2Exit)))  (
    VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
            VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("3", notBDD propA)])),
            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testRDEntry1 :: Test
testRDEntry1 = testRDEntry "PPA_1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exRD, rdExampleExit)))  (
    VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ])

testRDEntry2 :: Test
testRDEntry2 = testRDEntry "PPA_2" (VarPair(VarString (Var [("2", ttPC)]), VarPair(exRD, rdExampleExit))) (VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ])

testRDEntry4 :: Test
testRDEntry4 = testRDEntry "PPA_4" (VarPair(VarString (Var [("4", ttPC)]), VarPair(exRD, rdExampleExit))) (
    VarList[
        VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
        VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
        VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])), 
        VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ])   

testfindBlock :: String -> VarValor -> VarValor -> Test
testfindBlock name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/findBlock.lng" input
    -- putStrLn ("\n findBlock " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("findBlock " ++ name) (expectedOutput) (output)

testfindBlock1 :: Test
testfindBlock1 = testfindBlock "PPA 1" (VarPair(VarString (Var [("1", ttPC)]), exRD)) (encodeStmt rdS01)

testfindBlock2 :: Test
testfindBlock2 = testfindBlock "PPA 1" (VarPair(VarString (Var [("2", ttPC)]), exRD)) (encodeStmt rdS02)

testfindBlock4 :: Test
testfindBlock4 = testfindBlock "PPA 4" (VarPair(VarString (Var [("4", ttPC)]), exRD)) (encodeStmt rdWhileS1)

testfindBlock5 :: Test
testfindBlock5 = testfindBlock "PPA 5" (VarPair(VarString (Var [("5", ttPC)]), exRD)) (encodeStmt rdWhileS2)

testfindBlock2_v :: Test
testfindBlock2_v = testfindBlock "Ex2 2 21" (VarPair(VarString (Var [("2", propA), ("-2", notBDD propA)]), ex2)) (ex2_2)

testFindOrDefault :: String -> VarValor -> VarValor -> Test
testFindOrDefault name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/findOrDefault.lng" input
    -- putStrLn ("\n findOrDefault " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("findOrDefault " ++ name) (expectedOutput) (output)

testFindOrDefaultExitPPA2 :: Test
testFindOrDefaultExitPPA2 = testFindOrDefault "Exit PPA 2" (VarPair(VarString (Var [("2", ttPC)]), rdExampleExit)) (VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])) ])

testFindOrDefaultExitPPA6 :: Test
testFindOrDefaultExitPPA6 = testFindOrDefault "Exit PPA 6" (VarPair(VarString (Var [("6", ttPC)]), rdExampleExit)) (VarList[])

testFindOrDefaultEntryEx2_1 :: Test
testFindOrDefaultEntryEx2_1 = testFindOrDefault "Entry Ex2 1" (VarPair(VarString (Var [("1", ttPC)]), ex2Entry)) (VarList[
    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
    VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testFindOrDefaultEntryEx2_2_v :: Test
testFindOrDefaultEntryEx2_2_v = testFindOrDefault "Entry Ex2 2 A  21 ~A" (VarPair(VarString (Var [("2", propA), ("-2", notBDD propA)]), ex2Entry)) (VarList[
    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
    VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testGenRD1 :: Test
testGenRD1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/genRD.lng" (encodeStmt rdS01)
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])) ]
    assertEqual "test genRD 1" expectedOutput output

testGenRD2 :: Test
testGenRD2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/genRD.lng" (exRD)
    let expectedOutput = VarList[]
    assertEqual "test genRD factorial" expectedOutput output

testGenRDEx2_2 :: Test
testGenRDEx2_2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/genRD.lng" (ex2_2)
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])) ]
    -- putStrLn ("\n genRD Ex2 2: " ++ (substitute (show output)))
    assertEqual "test genRD Ex2 2" expectedOutput output

testKillRD :: String -> VarValor -> VarValor -> Test
testKillRD name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/killRD.lng" input
    -- putStrLn ("\n killRD " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("killRD " ++ name) (expectedOutput) (output)

testKillRDs01 :: Test
testKillRDs01 = testKillRD "s01" (VarPair(encodeStmt rdS01, exRD)) (VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])) ])

testKillRDEx2_v :: Test
testKillRDEx2_v = testKillRD "Ex2 21" (VarPair(ex2_2, ex2)) (VarList[
    VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
    VarPair(VarString (Var [("y", propA)]), VarString (Var [("?", propA)])) ])

testRDExit :: String -> VarValor -> VarValor -> Test
testRDExit name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/rdExit.lng" input
    -- putStrLn ("\n RDExit " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("RDExit " ++ name) (expectedOutput) (output)

testRDExit2_1 :: Test
testRDExit2_1 = testRDExit "Ex2_1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(ex2, ex2Entry)))  (
    VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testRDExit2_2 :: Test
testRDExit2_2 = testRDExit "Ex2_2" (VarPair(VarString (Var [("2", propA), ("-2", notBDD propA)]), VarPair(ex2, ex2Entry)))  (
    VarList[VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
            VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("?", notBDD propA)])),
            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testRDExit2_3 :: Test
testRDExit2_3 = testRDExit "Ex2_3" (VarPair(VarString (Var [("3", notBDD propA), ("-3", propA)]), VarPair(ex2, ex2Entry)))  (
    VarList[VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("3", notBDD propA)])),
            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testRDExit2_4 :: Test
testRDExit2_4 = testRDExit "Ex2_4" (VarPair(VarString (Var [("4", ttPC)]), VarPair(ex2, ex2Entry)))  (
    VarList[VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("4", ttPC)])),
            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
            VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("3", notBDD propA)])) ])

testRDExit1 :: Test
testRDExit1 = testRDExit "Ex1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exRD, rdExampleEntry))) (
    VarList[VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ])

testRDExit2 :: Test
testRDExit2 = testRDExit "Ex2" (VarPair(VarString (Var [("2", ttPC)]), VarPair(exRD, rdExampleEntry))) (VarList[
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])), 
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])) ])

testRDExit4 :: Test
testRDExit4 = testRDExit "Ex4" (VarPair(VarString (Var [("4", ttPC)]), VarPair(exRD, rdExampleEntry))) (VarList[
                        VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])),
                        VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                        VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])) ])

testLabels :: Test
testLabels = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/labels.lng" (ex2)
    let expectedOutput = VarList[VarString (Var [("1", ttPC)]), VarString (Var [("2", propA), ("-2", notBDD propA)]), VarString (Var [("3", notBDD propA), ("-3", propA)]), VarString (Var [("4", ttPC)])]
    assertEqual "labels" expectedOutput output

testInsertInto1 :: Test
testInsertInto1 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/insertIntoMap.lng" (VarPair(
                        VarString (Var [("2", ttPC)]),
                        VarPair(
                            VarList [
                                VarPair(VarString (Var [("teste", ttPC)]), VarString (Var [("teste", ttPC)])), 
                                VarPair(VarString (Var [("teste2", ttPC)]), VarString (Var [("teste2", ttPC)]))],
                            rdExampleEntry
                        )))
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("1", ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])), 
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
                            VarPair(VarString (Var [("2", ttPC)]), VarList[
                                                    VarPair(VarString (Var [("teste", ttPC)]), VarString (Var [("teste", ttPC)])), 
                                                    VarPair(VarString (Var [("teste2", ttPC)]), VarString (Var [("teste2", ttPC)])),
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
                            VarPair(VarString (Var [("3", ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])),
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ]),
                            VarPair(VarString (Var [("4", ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("2", ttPC)])), 
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ]),
                            VarPair(VarString (Var [("5", ttPC)]), VarList[
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("5", ttPC)])),
                                                    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ])]
    -- putStrLn ("\n testInsertInto : " ++ (substitute (show output)))
    assertEqual "testInsertInto" expectedOutput output

testInsertInto2 :: Test
testInsertInto2 = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/insertIntoMap.lng" (VarPair(
                        VarString (Var [("2", propA), ("-2", notBDD propA)]),
                        VarPair(
                            VarList[VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                                    VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("?", notBDD propA)])),
                                    VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ],
                            ex2Exit
                        )))
    let expectedOutput = ex2Exit
    -- putStrLn ("\n testInsertInto2 : " ++ (substitute (show output)))
    assertEqual "testInsertInto" expectedOutput output

testUpdateMappings :: Test
testUpdateMappings = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/updateMappings.lng" (ex2)
    let expectedOutput = VarPair(ex2Entry, ex2Exit)
    assertEqual "testUpdateMappings" expectedOutput output

testReachingDefinitions :: String -> VarValor -> VarValor -> Test
testReachingDefinitions name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/reachingDefinitions.lng" input
    -- putStrLn ("\n ReachingDefinitions " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("ReachingDefinitions " ++ name) expectedOutput output

testReachingDefinitionsEx2 :: Test
testReachingDefinitionsEx2 = testReachingDefinitions "Ex2"  (VarPair(VarInteger (Var [(10, ttPC)]), ex2)) (VarPair(ex2Entry, ex2Exit))

testReachingDefinitionsPPA :: Test
testReachingDefinitionsPPA = testReachingDefinitions "PPA"  (VarPair(VarInteger (Var [(10, ttPC)]), exRD)) (VarPair(rdExampleEntry, rdExampleExit))

testUnion :: String -> VarValor -> VarValor -> Test
testUnion name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/union.lng" input
    -- putStrLn ("\n union " ++ name ++ ", result: " ++ (substitute (show output)))
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

testUnion7 :: Test
testUnion7 = testUnion "VarList {list = [VarString {str = {('a', A), ('b', ~A)}}]} and VarList {list = [VarString {str = {('a', ~A), ('b', A)}}]}" (VarPair(VarList [VarString (Var [("a", propA), ("b", notBDD propA)])], VarList [VarString (Var [("a", notBDD propA), ("b", propA)])])) (VarList [VarString (Var [("a", notBDD propA), ("b", propA)]), VarString (Var [("a", propA), ("b", notBDD propA)])]) 

testUnion8 :: Test
testUnion8 = testUnion "VarList {list = [VarString {str = {('a', A), ('b', ~A)}}]} and VarList {list = [VarString {str = {('a', A), ('b', ~A)}}]}" (VarPair(VarList [VarString (Var [("a", propA), ("b", notBDD propA)])], VarList [VarString (Var [("a", propA), ("b", notBDD propA)])])) (VarList [VarString (Var [("a", propA), ("b", notBDD propA)])]) 

testIntersection :: String -> VarValor -> VarValor -> Test
testIntersection name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/intersection.lng" input
    -- putStrLn ("\n intersection " ++ name ++ ", result: " ++ (substitute (show output)))
    assertEqual ("intersection " ++ name) expectedOutput output

testIntersection1 :: Test
testIntersection1 = testIntersection "1: VarList {list = [VarString {str = {('a', tt)}}]} and VarList {list = [VarString {str = {('a', tt)}}]}" (VarPair(VarList [VarString (Var [("a", ttPC)])], VarList [VarString (Var [("a", ttPC)])])) (VarList [VarString (Var [("a", ttPC)])]) 

testIntersection2 :: Test
testIntersection2 = testIntersection "2: VarList {list = [VarString {str = {('a', A)}}]} and VarList {list = [VarString {str = {('a', tt)}}]}" (VarPair(VarList [VarString (Var [("a", propA)])], VarList [VarString (Var [("a", ttPC)])])) (VarList [VarString (Var [("a", propA)])]) 

testIntersection2_2 :: Test
testIntersection2_2 = testIntersection "2_2: VarList {list = [VarString {str = {('a', tt)}}]} and VarList {list = [VarString {str = {('a', A)}}]}" (VarPair(VarList [VarString (Var [("a", ttPC)])], VarList [VarString (Var [("a", propA)])])) (VarList [VarString (Var [("a", propA)])]) 

testIntersection3 :: Test
testIntersection3 = testIntersection "3: VarList {list = [VarString {str = {('a', ~A)}}]} and VarList {list = [VarString {str = {('a', tt)}}]}" (VarPair(VarList [VarString (Var [("a", notBDD propA)])], VarList [VarString (Var [("a", ttPC)])])) (VarList [VarString (Var [("a", notBDD propA)])]) 

testIntersection4 :: Test
testIntersection4 = testIntersection "4: VarList {list = [VarString {str = {('a', A)}}]} and VarList {list = [VarString {str = {('a', ~A)}}]}" (VarPair(VarList [VarString (Var [("a", propA)])], VarList [VarString (Var [("a", notBDD propA)])])) (VarList []) 

testIntersection5 :: Test
testIntersection5 = testIntersection "5: VarList {list = [VarString {str = {('a', tt)}}]} and VarList {list = [VarString {str = {('a', ff)}}]}" (VarPair(VarList [VarString (Var [("a", ttPC)])], VarList [VarString (Var [("a", ffPC)])])) (VarList []) 

testIntersection6 :: Test
testIntersection6 = testIntersection "6: VarList {list = [VarString {str = {('a', A)}}]} and VarList {list = [VarString {str = {('a', ff)}}]}" (VarPair(VarList [VarString (Var [("a", propA)])], VarList [VarString (Var [("a", ffPC)])])) (VarList []) 

testIntersection7 :: Test
testIntersection7 = testIntersection "7: VarList {list = [VarString {str = {('a', A), ('b', ~A)}}]} and VarList {list = [VarString {str = {('a', ~A), ('b', A)}}]}" (VarPair(VarList [VarString (Var [("a", propA), ("b", notBDD propA)])], VarList [VarString (Var [("a", notBDD propA), ("b", propA)])])) (VarList []) 

testIntersection8 :: Test
testIntersection8 = testIntersection "8: VarList {list = [VarString {str = {('a', A), ('b', ~A)}}]} and VarList {list = [VarString {str = {('a', A), ('b', ~A)}}]}" (VarPair(VarList [VarString (Var [("a", propA), ("b", notBDD propA)])], VarList [VarString (Var [("a", propA), ("b", notBDD propA)])])) (VarList [VarString (Var [("a", propA), ("b", notBDD propA)])]) 

testIsMember :: String -> VarValor -> VarValor -> Test
testIsMember name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/isMember.lng" input
    -- putStrLn ("\n isMember " ++ name ++ ", result: " ++ (substitute (show output)))
    assertEqual ("isMember " ++ name) (show expectedOutput) (show output)

testIsMember1 :: Test
testIsMember1 = testIsMember "1: VarString {str = {('a', tt)}} and VarList {list = [VarString {str = {('a', tt)}}]}" (VarPair(VarString (Var [("a", ttPC)]), VarList [VarString (Var [("a", ttPC)])])) (VarInteger (Var [(1, ttPC)])) 

testIsMember2 :: Test
testIsMember2 = testIsMember "2: VarString {str = {('a', A)}} and VarList {list = [VarString {str = {('a', tt)}}]}" (VarPair(VarString (Var [("a", propA)]), VarList [VarString (Var [("a", ttPC)])])) (VarInteger (Var [(1, propA)]))

testIsMember2_2 :: Test
testIsMember2_2 = testIsMember "2_2: VarString {str = {('a', tt)}} and VarList {list = [VarString {str = {('a', A)}}]}" (VarPair(VarString (Var [("a", ttPC)]), VarList [VarString (Var [("a", propA)])])) (VarInteger (Var [(1, ttPC)])) 

testIsMember3 :: Test
testIsMember3 = testIsMember "3: VarString {str = {('a', ~A)}} and VarList {list = [VarString {str = {('a', tt)}}]}" (VarPair(VarString (Var [("a", notBDD propA)]), VarList [VarString (Var [("a", ttPC)])])) (VarInteger (Var [(1, notBDD propA)])) 

testIsMember4 :: Test
testIsMember4 = testIsMember "4: VarString {str = {('a', A)}} and VarList {list = [VarString {str = {('a', ~A)}}]}" (VarPair(VarString (Var [("a", propA)]), VarList [VarString (Var [("a", notBDD propA)])])) (VarInteger (Var [(0, propA)]))  

testIsMember5 :: Test
testIsMember5 = testIsMember "5: VarString {str = {('a', tt)}} and VarList {list = [VarString {str = {('a', ff)}}]}" (VarPair(VarString (Var [("a", ttPC)]), VarList [VarString (Var [("a", ffPC)])])) (VarInteger (Var [(0, ttPC)])) 

testIsMember6 :: Test
testIsMember6 = testIsMember "6: VarString {str = {('a', A)}} and VarList {list = [VarString {str = {('a', ff)}}]}" (VarPair(VarString (Var [("a", propA)]), VarList [VarString (Var [("a", ffPC)])])) (VarInteger (Var [(0, propA)])) 

testIsMember7 :: Test
testIsMember7 = testIsMember "7: VarString {str = {('a', A), ('b', ~A)}} and VarList {list = [VarString {str = {('a', A), ('c', ~A)}}]}" (VarPair(VarString (Var [("a", propA), ("b", notBDD propA)]), VarList [VarString (Var [("a", propA), ("c", notBDD propA)])])) (VarInteger (Var [(1, propA), (0, notBDD propA)])) 

testIsMember8 :: Test
testIsMember8 = testIsMember "8: VarString {str = {('a', A), ('b', ~A)}} and VarList {list = [VarString {str = {('a', A), ('b', ~A)}}]}" (VarPair(VarString (Var [("a", propA), ("b", notBDD propA)]), VarList [VarString (Var [("a", propA), ("b", notBDD propA)])])) (VarInteger (Var [(1, ttPC)]))

testLVEntry :: String -> VarValor -> VarValor -> Test
testLVEntry name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/lvEntry.lng" input
    -- putStrLn ("\n LVEntry " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("LVEntry " ++ name) (show expectedOutput) (show output)

testLVEntry1 :: Test
testLVEntry1 = testLVEntry "ExPPA 1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[])

testLVEntry2 :: Test
testLVEntry2 = testLVEntry "ExPPA 2" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[])

testLVEntry3 :: Test
testLVEntry3 = testLVEntry "ExPPA 3" (VarPair(VarString (Var [("3", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[VarString (Var [("y", ttPC)])])

testLVEntry4 :: Test
testLVEntry4 = testLVEntry "ExPPA 4" (VarPair(VarString (Var [("4", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)])])

testLVEntry5 :: Test
testLVEntry5 = testLVEntry "ExPPA 5" (VarPair(VarString (Var [("5", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[VarString (Var [("y", ttPC)])])

testLVEntry6 :: Test
testLVEntry6 = testLVEntry "ExPPA 6" (VarPair(VarString (Var [("6", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[VarString (Var [("y", ttPC)])])

testLVEntry7 :: Test
testLVEntry7 = testLVEntry "ExPPA 7" (VarPair(VarString (Var [("7", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[VarString (Var [("z", ttPC)])])

testLiveVariables :: String -> VarValor -> VarValor -> Test
testLiveVariables name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/liveVariables.lng" input
    -- putStrLn ("\n LiveVariables " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("LiveVariables " ++ name) expectedOutput output

testLiveVariablesExPPA :: Test
testLiveVariablesExPPA = testLiveVariables "ExPPA LV"  (VarPair(VarInteger (Var [(10, ttPC)]), exLV)) (VarPair(lvExampleEntry, lvExampleExit))

testAEEntry :: String -> VarValor -> VarValor -> Test
testAEEntry name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/aeEntry.lng" input
    -- putStrLn ("\n AEEntry " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("AEEntry " ++ name) (show expectedOutput) (show output)

testAEEntry1 :: Test
testAEEntry1 = testAEEntry "ExPPA 1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exAE, aeExampleExit)))  (
    VarList[])

testAEEntry2 :: Test
testAEEntry2 = testAEEntry "ExPPA 2" (VarPair(VarString (Var [("2", ttPC)]), VarPair(exAE, aeExampleExit)))  (
    VarList[a_plus_b])

testAEEntry3 :: Test
testAEEntry3 = testAEEntry "ExPPA 3" (VarPair(VarString (Var [("3", ttPC)]), VarPair(exAE, aeExampleExit)))  (
    VarList[a_plus_b])

testAEEntry4 :: Test
testAEEntry4 = testAEEntry "ExPPA 4" (VarPair(VarString (Var [("4", ttPC)]), VarPair(exAE, aeExampleExit)))  (
    VarList[a_plus_b])

testAEEntry5 :: Test
testAEEntry5 = testAEEntry "ExPPA 5" (VarPair(VarString (Var [("5", ttPC)]), VarPair(exAE, aeExampleExit)))  (
    VarList[])

testAEExit :: String -> VarValor -> VarValor -> Test
testAEExit name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/aeExit.lng" input
    -- putStrLn ("\n AEExit " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("AEExit " ++ name) (expectedOutput) (output)

testAEExit1 :: Test
testAEExit1 = testAEExit "ExPPA 1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exAE, aeExampleEntry)))  (
    VarList[a_plus_b])

testAEExit2 :: Test
testAEExit2 = testAEExit "ExPPA 2" (VarPair(VarString (Var [("2", ttPC)]), VarPair(exAE, aeExampleEntry)))  (
    VarList[a_mult_b, a_plus_b])

testAEExit3 :: Test
testAEExit3 = testAEExit "ExPPA 3" (VarPair(VarString (Var [("3", ttPC)]), VarPair(exAE, aeExampleEntry)))  (
    VarList[a_plus_b])

testAEExit4 :: Test
testAEExit4 = testAEExit "ExPPA 4" (VarPair(VarString (Var [("4", ttPC)]), VarPair(exAE, aeExampleEntry)))  (
    VarList[])

testAEExit5 :: Test
testAEExit5 = testAEExit "ExPPA 5" (VarPair(VarString (Var [("5", ttPC)]), VarPair(exAE, aeExampleEntry)))  (
    VarList[a_plus_b])

testAvailableExpressions :: String -> VarValor -> VarValor -> Test
testAvailableExpressions name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/availableExpressions.lng" input
    -- putStrLn ("\n AvailableExpressions " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("AvailableExpressions " ++ name) expectedOutput output

testAvailableExpressionsExPPA :: Test
testAvailableExpressionsExPPA = testAvailableExpressions "ExPPA AE"  (VarPair(VarInteger (Var [(10, ttPC)]), exAE)) (VarPair(aeExampleEntry, aeExampleExit))

testVBExit :: String -> VarValor -> VarValor -> Test
testVBExit name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/vbExit.lng" input
    -- putStrLn ("\n VBExit " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("VBExit " ++ name) (expectedOutput) (output)

testVBExit1 :: Test
testVBExit1 = testVBExit "ExPPA 1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exVB, vbExampleEntry)))  (
    VarList[a_minus_b, b_minus_a])

testVeryBusyExpressions :: String -> VarValor -> VarValor -> Test
testVeryBusyExpressions name input expectedOutput = TestCase $ do
    output <- processFile executeProg "src/Language/Examples/DFA/veryBusyExpressions.lng" input
    -- putStrLn ("\n VeryBusyExpressions " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("VeryBusyExpressions " ++ name) expectedOutput output

testVeryBusyExpressionsExPPA :: Test
testVeryBusyExpressionsExPPA = testVeryBusyExpressions "ExPPA VB"  (VarPair(VarInteger (Var [(10, ttPC)]), exVB)) (VarPair(vbExampleEntry, vbExampleExit))

varDFATestSuite :: Test
varDFATestSuite = TestList [    TestLabel "is pair" testIsPair
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
                        ,   TestLabel "Count Asgns exRD" testCountexRD
                        ,   TestLabel "Init Ex2_1" testInitEx2_1
                        ,   TestLabel "Init Ex2_2" testInitEx2_2
                        ,   TestLabel "Init Ex2_3" testInitEx2_3
                        ,   TestLabel "Init Ex2_4" testInitEx2_4
                        ,   TestLabel "Init ex1" testInitEx1
                        ,   TestLabel "Init ex2" testInitEx2
                        ,   TestLabel "Init exRD" testInitexRD
                        ,   TestLabel "Final ex1" testFinalEx1
                        ,   TestLabel "Final ex2" testFinalEx2
                        ,   TestLabel "Final ex2_1" testFinalEx2_1
                        ,   TestLabel "Final ex2_2" testFinalEx2_2
                        ,   TestLabel "Final ex2_3" testFinalEx2_3
                        ,   TestLabel "Final ex2_4" testFinalEx2_4
                        ,   TestLabel "Final exRD" testFinalexRD
                        ,   TestLabel "Flow ex1" testFlowEx1
                        ,   TestLabel "Flow ex2" testFlowEx2
                        ,   TestLabel "Flow exRD" testFlowexRD
                        ,   TestLabel "FlowR ex1" testFlowREx1
                        ,   TestLabel "FlowR ex2" testFlowREx2
                        ,   TestLabel "FlowR exRD" testFlowRexRD
                        ,   TestLabel "Chaotic Iteration 1" testChaoticIteration1
                        ,   TestLabel "Chaotic Iteration 2" testChaoticIteration2
                        ,   TestLabel "Asgns ex1" testAssignmentsEx1
                        ,   TestLabel "Asgns ex2_1" testAssignmentsEx2_1
                        ,   TestLabel "Asgns ex2_2" testAssignmentsEx2_2
                        ,   TestLabel "Asgns ex2" testAssignmentsEx2
                        ,   TestLabel "Asgns exRD" testAssignmentsexRD
                        ,   TestLabel "fv ex1" testfvEx1
                        ,   TestLabel "fv ex2_1" testfvEx2_1
                        ,   TestLabel "fv ex2_2" testfvEx2_2
                        ,   TestLabel "fv ex2_3" testfvEx2_3
                        ,   TestLabel "fv ex2_4" testfvEx2_4
                        ,   TestLabel "fv ex2" testfvEx2
                        ,   TestLabel "fv exRD" testfvexRD
                        ,   TestLabel "testmakeSetOfFVEx1" testmakeSetOfFVEx1
                        ,   TestLabel "testmakeSetOfFVEx2" testmakeSetOfFVEx2
                        ,   TestLabel "testFilterFlow" testFilterFlow
                        ,   TestLabel "testFilterFlow Ex2 2" testFilterFlowEx2_2
                        ,   TestLabel "testFilterFlow Ex2 3" testFilterFlowEx2_3
                        ,   TestLabel "testFilterFlow Ex2 31" testFilterFlowEx2_31
                        ,   TestLabel "testFilterFlow Ex2 4" testFilterFlowEx2_4
                        ,   TestLabel "rdEntry 2 - 1" testRDEntry2_1
                        ,   TestLabel "rdEntry 2 - 2" testRDEntry2_2
                        ,   TestLabel "rdEntry 2 - 3" testRDEntry2_3
                        ,   TestLabel "rdEntry 2 - 4" testRDEntry2_4
                        ,   TestLabel "rdEntry 1" testRDEntry1
                        ,   TestLabel "rdEntry 2" testRDEntry2
                        ,   TestLabel "rdEntry 4" testRDEntry4
                        ,   TestLabel "testfindBlock 1" testfindBlock1
                        ,   TestLabel "testfindBlock 2" testfindBlock2
                        ,   TestLabel "testfindBlock 4" testfindBlock4
                        ,   TestLabel "testfindBlock 5" testfindBlock5
                        ,   TestLabel "testfindBlock -2" testfindBlock2_v
                        ,   TestLabel "testFindOrDefault Exit PPA 2" testFindOrDefaultExitPPA2
                        ,   TestLabel "testFindOrDefault Exit PPA 6" testFindOrDefaultExitPPA6
                        ,   TestLabel "testFindOrDefault Entry Ex2 1" testFindOrDefaultEntryEx2_1
                        ,   TestLabel "testFindOrDefault Entry Ex2 2 A -2 ~A" testFindOrDefaultEntryEx2_2_v
                        ,   TestLabel "testGenRD 1" testGenRD1
                        ,   TestLabel "testGenRD 2" testGenRD2
                        ,   TestLabel "testGenRD Ex2 2" testGenRDEx2_2
                        ,   TestLabel "testKillRD s01" testKillRDs01
                        ,   TestLabel "testKillRD Ex2 2" testKillRDEx2_v
                        ,   TestLabel "rdExit 2 - 1" testRDExit2_1
                        ,   TestLabel "rdExit 2 - 2" testRDExit2_2
                        ,   TestLabel "rdExit 2 - 3" testRDExit2_3
                        ,   TestLabel "rdExit 2 - 4" testRDExit2_4
                        ,   TestLabel "rdExit 1" testRDExit1
                        ,   TestLabel "rdExit 2" testRDExit2
                        ,   TestLabel "rdExit 4" testRDExit4
                        ,   TestLabel "labels" testLabels
                        ,   TestLabel "testInsertInto 1" testInsertInto1
                        ,   TestLabel "testInsertInto 2" testInsertInto2
                        ,   TestLabel "testReachingDefinitionsEx2" testReachingDefinitionsEx2
                        ,   TestLabel "testReachingDefinitionsPPA" testReachingDefinitionsPPA
                        ,   TestLabel "union 1" testUnion1
                        ,   TestLabel "union 2" testUnion2
                        ,   TestLabel "union 2_2" testUnion2_2
                        ,   TestLabel "union 3" testUnion3
                        ,   TestLabel "union 4" testUnion4
                        ,   TestLabel "union 5" testUnion5
                        ,   TestLabel "union 6" testUnion6
                        ,   TestLabel "union 7" testUnion7
                        ,   TestLabel "union 8" testUnion8
                        ,   TestLabel "intersection 1" testIntersection1
                        ,   TestLabel "intersection 2" testIntersection2
                        ,   TestLabel "intersection 2_2" testIntersection2_2
                        ,   TestLabel "intersection 3" testIntersection3
                        ,   TestLabel "intersection 4" testIntersection4
                        ,   TestLabel "intersection 5" testIntersection5
                        ,   TestLabel "intersection 6" testIntersection6
                        ,   TestLabel "intersection 7" testIntersection7
                        ,   TestLabel "intersection 8" testIntersection8
                        ,   TestLabel "isMember 1" testIsMember1
                        ,   TestLabel "isMember 2" testIsMember2
                        ,   TestLabel "isMember 2_2" testIsMember2_2
                        ,   TestLabel "isMember 3" testIsMember3
                        ,   TestLabel "isMember 4" testIsMember4
                        ,   TestLabel "isMember 5" testIsMember5
                        ,   TestLabel "isMember 6" testIsMember6
                        ,   TestLabel "isMember 7" testIsMember7
                        ,   TestLabel "isMember 8" testIsMember8
                        ,   TestLabel "testLVEntry1" testLVEntry1
                        ,   TestLabel "testLVEntry2" testLVEntry2
                        ,   TestLabel "testLVEntry3" testLVEntry3
                        ,   TestLabel "testLVEntry4" testLVEntry4
                        ,   TestLabel "testLVEntry5" testLVEntry5
                        ,   TestLabel "testLVEntry6" testLVEntry6
                        ,   TestLabel "testLVEntry7" testLVEntry7
                        ,   TestLabel "LiveVariablesExPPA" testLiveVariablesExPPA
                        ,   TestLabel "testAEEntry1" testAEEntry1
                        ,   TestLabel "testAEEntry2" testAEEntry2
                        ,   TestLabel "testAEEntry3" testAEEntry3
                        ,   TestLabel "testAEEntry4" testAEEntry4
                        ,   TestLabel "testAEEntry5" testAEEntry5
                        ,   TestLabel "testAEExit1" testAEExit1
                        ,   TestLabel "testAEExit2" testAEExit2
                        ,   TestLabel "testAEExit3" testAEExit3
                        ,   TestLabel "testAEExit4" testAEExit4
                        ,   TestLabel "testAEExit5" testAEExit5
                        ,   TestLabel "testAvailableExpressionsExPPA" testAvailableExpressionsExPPA
                        ,   TestLabel "testVeryBusyExpressionsExPPA" testVeryBusyExpressionsExPPA
                        ]