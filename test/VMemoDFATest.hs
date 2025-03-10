module VMemoDFATest where

import Language.VMemoInterpreter.Interpreter
import Language.VMemoInterpreter.Driver
import Variability.VarTypes (Prop, VarValor(..), Var (Var), propA, propB, atbt, atbf, afbt, afbf, apply, substitute, mkBDDVar, notBDD, ttPC, ffPC, tt, ff, (/\), (\/), (|||))

import Language.Frontend.ErrM
import Helper (processFile)

import Test.HUnit
import System.Timeout (timeout)
import Control.Exception (evaluate)

import WhileLang.WhileDFAExamples (rdS01, rdS02, rdWhileS1,rdWhileS2, rdExample, ex2While, lvExample, aeExample, vbExample)
import WhileLang.WhileEncoder (encodeStmt)
import VarDFAExamples (
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

testIsPair :: Test
testIsPair = TestCase $ do
    output <- processFile (executeProg ["check"] []) "src/Language/Analysis/DFA/isPair.lng" ex1
    let expectedOutput = (VarBool (Var [(True, ttPC)]))
    assertEqual "is ex1 pair" expectedOutput (fst output)

testCount :: String -> VarValor -> VarValor -> Test
testCount name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["count"] []) "src/Language/Analysis/DFA/Count-Asgns.lng" input
    -- putStrLn ("\n CountAsgns" ++ name ++ " result: " ++ (substitute (show (fst output))))
    -- putStrLn ("\n\n CountAsgns" ++ name ++ " memory: " ++ (substitute (show (snd output))))
    assertEqual ("Count Asgns " ++ name) expectedOutput (fst output)


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
    output <- processFile (executeProg ["init"] []) "src/Language/Analysis/DFA/init.lng" input
    assertEqual ("init " ++ name) expectedOutput (fst output)

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
    output <- processFile (executeProg ["final"] []) "src/Language/Analysis/DFA/final.lng" input
    -- putStrLn ("\n final out " ++ name ++ ": " ++ (substitute (show output)))
    assertEqual ("final " ++ name) expectedOutput (fst output)

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
    output <- processFile (executeProg ["flow"] []) "src/Language/Analysis/DFA/flow.lng" input
    -- putStrLn ("\n Flow out " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("flow " ++ name) expectedOutput (fst output)

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

testChaoticIteration1 :: Test
testChaoticIteration1 = TestCase $ do
    output <- processFile (executeProg ["chaoticIteration"] []) "src/Language/Analysis/DFA/chaoticIteration.lng" (VarPair(VarInteger (Var [(1, ttPC)]), VarInteger (Var [(2, ttPC)])))
    let expectedOutput = (VarInteger (Var [(3, ttPC)]))
    assertEqual "chaotic iteration 1 test" expectedOutput (fst output)

testChaoticIteration2 :: Test
testChaoticIteration2 = TestCase $ do
    output <- processFile (executeProg ["chaoticIteration"] []) "src/Language/Analysis/DFA/chaoticIteration.lng" (VarPair(VarInteger (Var [(2, ttPC)]), VarInteger (Var [(2, ttPC)])))
    let expectedOutput = (VarInteger (Var [(4, ttPC)]))
    assertEqual "chaotic iteration 2 test" expectedOutput (fst output)

testAssignments :: String -> VarValor -> VarValor -> Test
testAssignments name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["assignments"] []) "src/Language/Analysis/DFA/assignments.lng" input
    -- putStrLn ("\n Assignments " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("Assignments " ++ name) expectedOutput (fst output)

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
    output <- processFile (executeProg ["fv"] []) "src/Language/Analysis/DFA/fv.lng" input
    -- putStrLn ("\n fv " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("fv " ++ name) expectedOutput (fst output)

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
    output <- processFile (executeProg ["fv"] []) "src/Language/Analysis/DFA/makeSetOfFV.lng" input
    -- putStrLn ("\n makeSetOfFV " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("makeSetOfFV " ++ name) expectedOutput (fst output)

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
    output <- processFile (executeProg ["filterFlow"] []) "src/Language/Analysis/DFA/filterFlow.lng" input
    -- putStrLn ("\n FilterFlow " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("FilterFlow " ++ name) expectedOutput (fst output)

testFilterFlow :: Test
testFilterFlow = testFilterFlowBase "ex" (VarPair(VarString (Var [("3", ttPC)]), VarList[
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
    output <- processFile (executeProg ["flow"] []) "src/Language/Analysis/DFA/rdEntry.lng" input
    -- putStrLn ("\n RDEntry " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("RDEntry " ++ name) (show expectedOutput) (show (fst output))

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
    output <- processFile (executeProg ["findBlock"] []) "src/Language/Analysis/DFA/findBlock.lng" input
    -- putStrLn ("\n findBlock " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("findBlock " ++ name) (expectedOutput) (fst output)

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
    output <- processFile (executeProg ["findOrDefault"] []) "src/Language/Analysis/DFA/findOrDefault.lng" input
    -- putStrLn ("\n findOrDefault " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("findOrDefault " ++ name) (expectedOutput) (fst output)

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

testFindOrDefaultEntryEx2_2_21 :: Test
testFindOrDefaultEntryEx2_2_21 = testFindOrDefault "Entry Ex2 2 A  21 ~A" (VarPair(VarString (Var [("2", propA), ("-2", notBDD propA)]), ex2Entry)) (VarList[
    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
    VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])),
    VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ])

testGenRD1 :: Test
testGenRD1 = TestCase $ do
    output <- processFile (executeProg ["genRD"] []) "src/Language/Analysis/DFA/genRD.lng" (encodeStmt rdS01)
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])) ]
    assertEqual "test genRD 1" expectedOutput (fst output)

testGenRD2 :: Test
testGenRD2 = TestCase $ do
    output <- processFile (executeProg ["genRD"] []) "src/Language/Analysis/DFA/genRD.lng" (exRD)
    let expectedOutput = VarList[]
    assertEqual "test genRD" expectedOutput (fst output)

testGenRDEx2_2 :: Test
testGenRDEx2_2 = TestCase $ do
    output <- processFile (executeProg ["genRD"] []) "src/Language/Analysis/DFA/genRD.lng" (ex2_2)
    let expectedOutput = VarList[
                            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])) ]
    -- putStrLn ("\n genRD Ex2 2: " ++ (substitute (show output)))
    assertEqual "test genRD Ex2 2" expectedOutput (fst output)

testKillRD :: String -> VarValor -> VarValor -> Test
testKillRD name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["assignments"] []) "src/Language/Analysis/DFA/killRD.lng" input
    -- putStrLn ("\n killRD " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("killRD " ++ name) (expectedOutput) (fst output)

testKillRDEx2_21 :: Test
testKillRDEx2_21 = testKillRD "Ex2 21" (VarPair(ex2_2, ex2)) (VarList[
    VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
    VarPair(VarString (Var [("y", propA)]), VarString (Var [("?", propA)])) ])

testRDExit :: String -> VarValor -> VarValor -> Test
testRDExit name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["assignments"] []) "src/Language/Analysis/DFA/rdExit.lng" input
    -- putStrLn ("\n RDExit " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("RDExit " ++ name) (expectedOutput) (fst output)

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
    output <- processFile (executeProg ["labels"] []) "src/Language/Analysis/DFA/labels.lng" (ex2)
    let expectedOutput = VarList[VarString (Var [("1", ttPC)]), VarString (Var [("2", propA), ("-2", notBDD propA)]), VarString (Var [("3", notBDD propA), ("-3", propA)]), VarString (Var [("4", ttPC)])]
    assertEqual "labels" expectedOutput (fst output)

testInsertInto :: String -> VarValor -> VarValor -> Test
testInsertInto name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["insertIntoMap"] []) "src/Language/Analysis/DFA/insertIntoMap.lng" input
    -- putStrLn ("\n testInsertInto " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("testInsertInto " ++ name) (expectedOutput) (fst output)

testInsertInto1 :: Test
testInsertInto1 = testInsertInto "Ex1" (VarPair(
                        VarString (Var [("2", ttPC)]),
                        VarPair(
                            VarList [
                                VarPair(VarString (Var [("teste", ttPC)]), VarString (Var [("teste", ttPC)])), 
                                VarPair(VarString (Var [("teste2", ttPC)]), VarString (Var [("teste2", ttPC)]))],
                            rdExampleEntry
                        ))) (VarList[
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
                                                        VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("4", ttPC)])) ])])

testInsertInto2 :: Test
testInsertInto2 = testInsertInto "Ex2" (VarPair(
                        VarString (Var [("2", propA), ("-2", notBDD propA)]),
                        VarPair(
                            VarList[VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                                    VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("?", notBDD propA)])),
                                    VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ],
                            ex2Exit
                        ))) (ex2Exit)

testInsertIntoEmpty :: Test
testInsertIntoEmpty = testInsertInto "Empty" (VarPair(
                        VarString (Var [("2", propA), ("-2", notBDD propA)]),
                        VarPair(
                            VarList[VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                                    VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                                    VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("?", notBDD propA)])),
                                    VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ],
                            VarList[]
                        ))) (VarList[
                                VarPair(VarString (Var [("2", propA), ("-2", notBDD propA)]), 
                                        VarList[
                                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])), 
                                            VarPair(VarString (Var [("y", propA)]), VarString (Var [("2", propA)])),
                                            VarPair(VarString (Var [("y", notBDD propA)]), VarString (Var [("?", notBDD propA)])),
                                            VarPair(VarString (Var [("z", ttPC)]), VarString (Var [("?", ttPC)])) ]) ])

testUpdateMappings :: Test
testUpdateMappings = TestCase $ do
    output <- processFile (executeProg ["flow"] []) "src/Language/Analysis/DFA/updateMappings.lng" (ex2)
    let expectedOutput = VarPair(ex2Entry, ex2Exit)
    assertEqual "testUpdateMappings" expectedOutput (fst output)

testReachingDefinitions :: String -> VarValor -> VarValor -> Test
testReachingDefinitions name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["labels", "flow", "fv", "assignments", "init", "final", "findBlock", "makeSetOfFV", "killRD", "genRD", "filterFlow"] []) "src/Language/Analysis/DFA/reachingDefinitions.lng" input
    -- output <- processFile (executeProg []) "src/Language/Analysis/DFA/reachingDefinitions.lng" input
    -- putStrLn ("\n ReachingDefinitions " ++ name ++ " : " ++ (substitute (show (fst output))))
    assertEqual ("ReachingDefinitions " ++ name) expectedOutput (fst output)

testReachingDefinitionsEx2 :: Test
testReachingDefinitionsEx2 = testReachingDefinitions "Ex2"  (ex2) (VarPair(ex2Entry, ex2Exit))

testReachingDefinitionsPPA :: Test
testReachingDefinitionsPPA = testReachingDefinitions "PPA"  (exRD) (VarPair(rdExampleEntry, rdExampleExit))

-- x = 1;  1
x1 = VarPair (
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

simple1 = x1

testFlowSimple1 = testFlow "simpl1" simple1 (VarList[])

entrySimple1 = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])) ])]

exitSimple1 = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])) ])]

testReachingDefinitionsSimple1 :: Test
testReachingDefinitionsSimple1 = testReachingDefinitions "Simple 1"  (simple1) (VarPair(entrySimple1, exitSimple1))

x2 = VarPair (
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarString (Var [("2", ttPC)]),
            VarPair (
                VarString (Var [("x", ttPC)]),
                VarPair(
                    VarString (Var [("INT", ttPC)]),
                    VarString (Var [("2", ttPC)])
                )
            )
        )
    ) -- x = 2;  tt    2

-- x = 1;  1
-- x = 2;  2
simple2 = VarPair(
            VarString (Var [("SEQ", ttPC)]),
            VarPair (
                x1,
                x2
            )
        )

entrySimple2 = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])) ]) ]

exitSimple2 = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])) ]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("2", ttPC)])) ]) ]

testReachingDefinitionsSimple2 :: Test
testReachingDefinitionsSimple2 = testReachingDefinitions "Simple 2"  (simple2) (VarPair(entrySimple2, exitSimple2))

y3 = VarPair (
        VarString (Var [("ASGN", ttPC)]),
        VarPair (
            VarString (Var [("3", ttPC)]),
            VarPair (
                VarString (Var [("y", ttPC)]),
                VarPair(
                    VarString (Var [("INT", ttPC)]),
                    VarString (Var [("3", ttPC)])
                )
            )
        )
    ) -- y = 3;  tt    3
-- x = 1;  1
-- x = 2;  2
-- y = 3; 3
simple3 = VarPair(
            VarString (Var [("SEQ", ttPC)]),
            VarPair (
                x1,
                VarPair(
                    VarString (Var [("SEQ", ttPC)]),
                    VarPair(
                        x2,
                        y3
                    )
                )
            )
        )

entrySimple3 = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("?", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("2", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]) ]

exitSimple3 = VarList[
    VarPair(VarString (Var [("1", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("1", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("2", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("2", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("?", ttPC)])) ]),
    VarPair(VarString (Var [("3", ttPC)]), VarList[
                            VarPair(VarString (Var [("x", ttPC)]), VarString (Var [("2", ttPC)])),
                            VarPair(VarString (Var [("y", ttPC)]), VarString (Var [("3", ttPC)])) ]) ]

testReachingDefinitionsSimple3 :: Test
testReachingDefinitionsSimple3 = testReachingDefinitions "Simple 3"  (simple3) (VarPair(entrySimple3, exitSimple3))


testUnion :: String -> VarValor -> VarValor -> Test
testUnion name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["union"] []) "src/Language/Analysis/DFA/union.lng" input
    -- putStrLn ("\n union " ++ name ++ ", result: " ++ (substitute (show output)))
    assertEqual ("union " ++ name) expectedOutput (fst output)

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

testLVEntry :: String -> VarValor -> VarValor -> Test
testLVEntry name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["getVarFromAexp", "getVarFromBexp"] []) "src/Language/Analysis/DFA/lvEntry.lng" input
    -- putStrLn ("\n LVEntry " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("LVEntry " ++ name) (expectedOutput) (fst output)

testLVEntry1 :: Test
testLVEntry1 = testLVEntry "exRD 1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[])

testLVEntry2 :: Test
testLVEntry2 = testLVEntry "exRD 2" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[])

testLVEntry3 :: Test
testLVEntry3 = testLVEntry "exRD 3" (VarPair(VarString (Var [("3", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[VarString (Var [("y", ttPC)])])

testLVEntry4 :: Test
testLVEntry4 = testLVEntry "exRD 4" (VarPair(VarString (Var [("4", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[VarString (Var [("x", ttPC)]), VarString (Var [("y", ttPC)])])

testLVEntry5 :: Test
testLVEntry5 = testLVEntry "exRD 5" (VarPair(VarString (Var [("5", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[VarString (Var [("y", ttPC)])])

testLVEntry6 :: Test
testLVEntry6 = testLVEntry "exRD 6" (VarPair(VarString (Var [("6", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[VarString (Var [("y", ttPC)])])

testLVEntry7 :: Test
testLVEntry7 = testLVEntry "exRD 7" (VarPair(VarString (Var [("7", ttPC)]), VarPair(exLV, lvExampleExit)))  (
    VarList[VarString (Var [("z", ttPC)])])

testLiveVariables :: String -> VarValor -> VarValor -> Test
testLiveVariables name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["getVarFromAexp", "getVarFromBexp", "labels", "flowR", "flow", "fv", "init", "final", "findBlock", "killLV", "genLV", "filterFlow"] []) "src/Language/Analysis/DFA/liveVariables.lng" input
    -- putStrLn ("\n LiveVariables " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("LiveVariables " ++ name) expectedOutput (fst output)

testLiveVariablesexRD :: Test
testLiveVariablesexRD = testLiveVariables "exRD LV"  (exLV) (VarPair(lvExampleEntry, lvExampleExit))

testAEEntry :: String -> VarValor -> VarValor -> Test
testAEEntry name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["flow"] []) "src/Language/Analysis/DFA/aeEntry.lng" input
    -- putStrLn ("\n AEEntry " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("AEEntry " ++ name) expectedOutput (fst output)

testAEEntry1 :: Test
testAEEntry1 = testAEEntry "exRD 1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exAE, aeExampleExit)))  (
    VarList[])

testAEEntry2 :: Test
testAEEntry2 = testAEEntry "exRD 2" (VarPair(VarString (Var [("2", ttPC)]), VarPair(exAE, aeExampleExit)))  (
    VarList[a_plus_b])

testAEEntry3 :: Test
testAEEntry3 = testAEEntry "exRD 3" (VarPair(VarString (Var [("3", ttPC)]), VarPair(exAE, aeExampleExit)))  (
    VarList[a_plus_b])

testAEEntry4 :: Test
testAEEntry4 = testAEEntry "exRD 4" (VarPair(VarString (Var [("4", ttPC)]), VarPair(exAE, aeExampleExit)))  (
    VarList[a_plus_b])

testAEEntry5 :: Test
testAEEntry5 = testAEEntry "exRD 5" (VarPair(VarString (Var [("5", ttPC)]), VarPair(exAE, aeExampleExit)))  (
    VarList[])

testAEExit :: String -> VarValor -> VarValor -> Test
testAEExit name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["nonTrivialExpression"] []) "src/Language/Analysis/DFA/aeExit.lng" input
    -- putStrLn ("\n AEExit " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("AEExit " ++ name) (expectedOutput) (fst output)

testAEExit1 :: Test
testAEExit1 = testAEExit "exRD 1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exAE, aeExampleEntry)))  (
    VarList[a_plus_b])

testAEExit2 :: Test
testAEExit2 = testAEExit "exRD 2" (VarPair(VarString (Var [("2", ttPC)]), VarPair(exAE, aeExampleEntry)))  (
    VarList[a_mult_b, a_plus_b])

testAEExit3 :: Test
testAEExit3 = testAEExit "exRD 3" (VarPair(VarString (Var [("3", ttPC)]), VarPair(exAE, aeExampleEntry)))  (
    VarList[a_plus_b])

testAEExit4 :: Test
testAEExit4 = testAEExit "exRD 4" (VarPair(VarString (Var [("4", ttPC)]), VarPair(exAE, aeExampleEntry)))  (
    VarList[])

testAEExit5 :: Test
testAEExit5 = testAEExit "exRD 5" (VarPair(VarString (Var [("5", ttPC)]), VarPair(exAE, aeExampleEntry)))  (
    VarList[a_plus_b])

testAvailableExpressions :: String -> VarValor -> VarValor -> Test
testAvailableExpressions name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["nonTrivialExpression", "labels", "flow", "fv", "init", "final", "findBlock", "killAE", "genAE", "filterFlow"] []) "src/Language/Analysis/DFA/availableExpressions.lng" input
    -- putStrLn ("\n AvailableExpressions " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("AvailableExpressions " ++ name) expectedOutput (fst output)

testAvailableExpressionsexRD :: Test
testAvailableExpressionsexRD = testAvailableExpressions "exRD AE"  (exAE) (VarPair(aeExampleEntry, aeExampleExit))

testVBExit :: String -> VarValor -> VarValor -> Test
testVBExit name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["flow", "flowR"] []) "src/Language/Analysis/DFA/vbExit.lng" input
    -- putStrLn ("\n VBExit " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("VBExit " ++ name) (expectedOutput) (fst output)

testVBExit1 :: Test
testVBExit1 = testVBExit "exRD 1" (VarPair(VarString (Var [("1", ttPC)]), VarPair(exVB, vbExampleEntry)))  (
    VarList[a_minus_b, b_minus_a])

testVeryBusyExpressions :: String -> VarValor -> VarValor -> Test
testVeryBusyExpressions name input expectedOutput = TestCase $ do
    output <- processFile (executeProg ["labels", "flow", "flowR", "fv", "init", "final", "findBlock", "killVB", "genVB", "filterFlow"] []) "src/Language/Analysis/DFA/veryBusyExpressions.lng" input
    -- putStrLn ("\n VeryBusyExpressions " ++ name ++ " : " ++ (substitute (show output)))
    assertEqual ("VeryBusyExpressions " ++ name) expectedOutput (fst output)

testVeryBusyExpressionsexRD :: Test
testVeryBusyExpressionsexRD = testVeryBusyExpressions "exRD VB"  (exVB) (VarPair(vbExampleEntry, vbExampleExit))


vMemoDFATestSuite :: Test
vMemoDFATestSuite = TestList [    
                            TestLabel "is pair" testIsPair
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
                        ,   TestLabel "testfindBlock 2 -2" testfindBlock2_v
                        ,   TestLabel "testFindOrDefault Exit PPA 2" testFindOrDefaultExitPPA2
                        ,   TestLabel "testFindOrDefault Exit PPA 6" testFindOrDefaultExitPPA6
                        ,   TestLabel "testFindOrDefault Entry Ex2 1" testFindOrDefaultEntryEx2_1
                        ,   TestLabel "testFindOrDefault Entry Ex2 2 A 21 ~A" testFindOrDefaultEntryEx2_2_21
                        ,   TestLabel "testGenRD 1" testGenRD1
                        ,   TestLabel "testGenRD 2" testGenRD2
                        ,   TestLabel "testGenRD Ex2 2" testGenRDEx2_2
                        ,   TestLabel "testKillRD Ex2 2" testKillRDEx2_21
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
                        ,   TestLabel "testInsertInto Empty" testInsertIntoEmpty
                        ,   TestLabel "testReachingDefinitionsEx2" testReachingDefinitionsEx2
                        ,   TestLabel "testReachingDefinitionsPPA" testReachingDefinitionsPPA
                        ,   TestLabel "testFlowSimple1" testFlowSimple1
                        ,   TestLabel "testReachingDefinitionsSimple1" testReachingDefinitionsSimple1
                        ,   TestLabel "testReachingDefinitionsSimple2" testReachingDefinitionsSimple2
                        ,   TestLabel "testReachingDefinitionsSimple3" testReachingDefinitionsSimple3
                        ,   TestLabel "union 1" testUnion1
                        ,   TestLabel "union 2" testUnion2
                        ,   TestLabel "union 2_2" testUnion2_2
                        ,   TestLabel "union 3" testUnion3
                        ,   TestLabel "union 4" testUnion4
                        ,   TestLabel "union 5" testUnion5
                        ,   TestLabel "union 6" testUnion6
                        ,   TestLabel "union 7" testUnion7
                        ,   TestLabel "union 8" testUnion8
                        ]