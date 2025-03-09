module Benchmark where

import WhileLang.ComplexExamples
import WhileLang.RunningExample
import WhileLang.WhileEncoder
import System.CPUTime
import Control.DeepSeq (deepseq, NFData(..))
import System.Mem (performGC)
import System.IO
import Data.Time.Clock
import qualified Data.Map as Map
import qualified Language.Interpreter.Driver as BaseInterpreter
import qualified Language.MInterpreter.Driver as MInterpreter
import qualified Language.VInterpreter.Driver as VInterpreter
import qualified Language.VMemoInterpreter.Driver as VMemoInterpreter
import Base.Types (Valor(..))
import Variability.VarTypes (VarValor(..), Var(..), Val, PresenceCondition, sat, andBDD, valList, ttPC)
import Memoization.Core.Memory (KeyValueArray, FuncKey(..))
import GHC.Stats (getRTSStats, RTSStats(..))
import Paths_lifting_framework (getDataFileName)
import System.Directory (doesFileExist)
import System.IO.Error (tryIOError)
import Debug.Trace (trace)

-- Interpreters
data Interpreter = Base | Variational | Memoized | VMemoized
    deriving (Show, Eq)

type ExecutionResult = (NominalDiffTime, Integer, Int, Int, [Valor]) -- (Runtime, Memory, Cache Hits, Cache Reuse, Result)

newtype MemState v = MemState (KeyValueArray FuncKey v)
    deriving (Show, Read)
    

-- List of analyses with their respective memoized function names
analyses :: [(String, [String])]
analyses = [ ("src/Language/Examples/DFA/reachingDefinitions.lng", 
              ["labels", "flow", "fv", "assignments", "init", "final", "findBlock", "makeSetOfFV", "killRD", "genRD", "filterFlow"]),
             ("src/Language/Examples/DFA/liveVariables.lng", 
              ["getVarFromAexp", "getVarFromBexp", "labels", "flowR", "flow", "fv", "init", "final", "findBlock", "killLV", "genLV", "filterFlow"]),
             ("src/Language/Examples/DFA/availableExpressions.lng", 
              ["nonTrivialExpression", "labels", "flow", "fv", "init", "final", "findBlock", "killAE", "genAE", "filterFlow"]),
             ("src/Language/Examples/DFA/veryBusyExpressions.lng", 
              ["labels", "flow", "flowR", "fv", "init", "final", "findBlock", "killVB", "genVB", "filterFlow"])
           ]

-- Run analysis using appropriate interpreter
runAnalysis :: Interpreter -> (String, [String]) -> Program -> Either (MemState Valor) (MemState VarValor) -> IO (ExecutionResult, Either (MemState Valor) (MemState VarValor))
runAnalysis interpreter (analysisFile, memoizedFunctions) program memState = do
    filePath <- getDataFileName analysisFile
    content <- readFile filePath
    performGC -- Ensure garbage collection before timing
    startTime <- getCurrentTime
    let (result, newMemState) = case (interpreter, memState) of
            (Base, Left mem)      -> let (r, m) = evaluateInterpreter Base content program mem memoizedFunctions in (r, Left m)
            (Memoized, Left mem)  -> let (r, m) = evaluateInterpreter Memoized content program mem memoizedFunctions in (r, Left m)
            (Variational, Right mem) -> let (r, m) = evaluateInterpreterVariational Variational content program mem memoizedFunctions in (r, Right m)
            (VMemoized, Right mem)  -> let (r, m) = evaluateInterpreterVariational VMemoized content program mem memoizedFunctions in (r, Right m)
            _ -> error ("Invalid memory state type for the chosen interpreter: " ++ show interpreter ++ ", " ++ show memState)
    deepseq result (return ()) -- Force evaluation to avoid lazy computation issues
    endTime <- getCurrentTime
    let runtime = diffUTCTime endTime startTime
    memoryUsage <- getMemoryUsage
    let (cacheHits, cacheReuse) = case newMemState of
            Left mem -> getMemoizationStats Memoized mem
            Right mem -> getMemoizationStats VMemoized mem
    return ((runtime, memoryUsage, cacheHits, cacheReuse, result), newMemState)

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _ = error "Expected Left value"

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Expected Right value"

evaluateInterpreter :: Interpreter -> String -> Program -> MemState Valor -> [String] -> ([Valor], MemState Valor)
evaluateInterpreter Base analysis program _ _ = 
    let encodedInputs = encodeVariability program
        results = map (BaseInterpreter.executeProg analysis) encodedInputs
    in (results, MemState [])
evaluateInterpreter Memoized analysis program (MemState memState) memoizedFunctions = 
    let encodedInputs = encodeVariability program
        (results, finalMemState) = foldl (\(accResults, accMem) input -> 
            let (res, newMem) = MInterpreter.executeProg memoizedFunctions accMem analysis input
            in (accResults ++ [res], newMem)
          ) ([], memState) encodedInputs
    in (results, MemState finalMemState)

evaluateInterpreterVariational :: Interpreter -> String -> Program -> MemState VarValor -> [String] -> ([Valor], MemState VarValor)
evaluateInterpreterVariational Variational analysis program _ _ = 
    let varResult = VInterpreter.executeProg analysis (encodeStmt program)
    in (toValorList varResult, MemState [])
evaluateInterpreterVariational VMemoized analysis program (MemState memState) memoizedFunctions = 
    let (varResult, newMemState) = VMemoInterpreter.executeProg memoizedFunctions memState analysis (encodeStmt program)
    in (toValorList varResult, MemState newMemState)

-- Collect memory usage
getMemoryUsage :: IO Integer
getMemoryUsage = do
    stats <- getRTSStats
    return (fromIntegral (max_live_bytes stats))

-- Function to track memoization statistics
getMemoizationStats :: Interpreter -> MemState v -> (Int, Int)
getMemoizationStats Memoized (MemState memState) = (length memState, 0)
getMemoizationStats VMemoized (MemState memState) = (length memState, 0)
getMemoizationStats _ _ = (0, 0)

saveMemoryState :: (Read v, Show v) => Interpreter -> MemState v -> IO ()
saveMemoryState Memoized memState = writeFile "memoized_state.dat" (show memState)
saveMemoryState VMemoized memState = writeFile "vmemoized_state.dat" (show memState)
saveMemoryState _ _ = return ()

loadMemoryState :: (Read v, Show v) => Interpreter -> IO (MemState v)
loadMemoryState Memoized = do
    exists <- doesFileExist "memoized_state.dat"
    if exists
        then do
            contentResult <- tryIOError (readFile "memoized_state.dat")
            case contentResult of
                Left _ -> return (MemState [])  -- Return an empty state on error
                Right content -> case reads content of
                    [(mem, "")] -> return mem
                    _ -> return (MemState [])  -- Avoid crashing if parsing fails
        else return (MemState [])
loadMemoryState VMemoized = do
    exists <- doesFileExist "vmemoized_state.dat"
    if exists
        then do
            contentResult <- tryIOError (readFile "vmemoized_state.dat")
            case contentResult of
                Left _ -> return (MemState []) 
                Right content -> case reads content of
                    [(mem, "")] -> return mem
                    _ -> return (MemState [])  
        else return (MemState [])
loadMemoryState _ = return (MemState [])  -- Base and Variational have no memoization


-- Function to write results
writeResults :: [Interpreter] -> [(String, [ExecutionResult], [ExecutionResult])] -> IO ()
writeResults interpreters results = do
    withFile "benchmark_results.csv" WriteMode $ \h -> do
        hPutStrLn h "Program,Interpreter,Version,Runtime,Memory,CacheHits,CacheReuse,Results,Result vs Base"
        mapM_ (writeEntries h) results
  where
    writeEntries h (name, res1, res2) =
        let baseResults1 = case lookup Base (zip interpreters res1) of
                            Just (_, _, _, _, baseRes) -> baseRes
                            Nothing -> []
            baseResults2 = case lookup Base (zip interpreters res2) of
                            Just (_, _, _, _, baseRes) -> baseRes
                            Nothing -> []
        in mapM_ (\(interpreter, res1Entry, res2Entry) -> writeEntry h name interpreter res1Entry res2Entry baseResults1 baseResults2)
                 (zip3 interpreters res1 res2)

    writeEntry h name interpreter (runtime1, memory1, cacheHits1, cacheReuse1, results1)
                               (runtime2, memory2, cacheHits2, cacheReuse2, results2) baseResults1 baseResults2 =
        let baseComparison1 = show (results1 == baseResults1)
            baseComparison2 = show (results1 == baseResults2)
        in hPutStrLn h $ name ++ "," ++ show interpreter ++ ",V1," ++ show runtime1 ++ "," ++ show memory1 ++ "," ++ show cacheHits1 ++ "," ++ show cacheReuse1 ++ "," ++ show results1 ++ "," ++ baseComparison1 ++ "\n" ++
                     name ++ "," ++ show interpreter ++ ",V2," ++ show runtime2 ++ "," ++ show memory2 ++ "," ++ show cacheHits2 ++ "," ++ show cacheReuse2 ++ "," ++ show results2 ++ "," ++ baseComparison2

-- Run experiments for all interpreters and analyses
runExperiments :: IO ()
runExperiments = do
    let programs = [("Running-Example", running_example_variability, running_example_variability_v2),
                    ("Deep Loop", deep_loop, deep_loop_v2),
                    ("Nested Loop", nested_variability, nested_variability_v2),
                    ("Interprocedural", interprocedural_sim, interprocedural_sim_v2),
                    ("Recursion Sim", factorial_rec_sim, factorial_rec_sim_v2),
                    ("Arithmetic Heavy", arithmetic_heavy, arithmetic_heavy_v2)]
    
    let interpreters = [Base, Variational, Memoized, VMemoized]
    
    results <- mapM (\(name, prog1, prog2) -> do
        putStrLn $ "Running " ++ name ++ "..."
    
        res1 <- mapM (\i -> do
            memState <- case i of
                Base -> Left <$> loadMemoryState Base
                Memoized -> Left <$> loadMemoryState Memoized
                Variational -> Right <$> loadMemoryState Variational
                VMemoized -> Right <$> loadMemoryState VMemoized
            
            runAnalysis i (head analyses) prog1 memState
            ) interpreters

        -- Save memory only for Memoized and VMemoized interpreters
        mapM_ (\(i, (_, mem)) -> case mem of
            Left m -> saveMemoryState Memoized m
            Right m -> saveMemoryState VMemoized m
            ) (zip interpreters res1)

        -- Load memory again
        res2 <- mapM (\i -> do
            memState <- case i of
                Base -> Left <$> loadMemoryState Memoized
                Memoized -> Left <$> loadMemoryState Memoized
                Variational -> Right <$> loadMemoryState VMemoized
                VMemoized -> Right <$> loadMemoryState VMemoized
            
            runAnalysis i (head analyses) prog2 memState
            ) interpreters

        return (name, map fst res1, map fst res2)
        ) programs
    
    writeResults interpreters results

expandVar :: Var a -> [(a, PresenceCondition)]
expandVar (Var vals) = [(v, pc) | (v, pc) <- vals, sat pc]

expandVarValor :: VarValor -> [(Valor, PresenceCondition)]
expandVarValor (VarInteger v) = [(ValorInt i, pc) | (i, pc) <- expandVar v]
expandVarValor (VarBool v) = [(ValorBool b, pc) | (b, pc) <- expandVar v]
expandVarValor (VarString v) = [(ValorStr s, pc) | (s, pc) <- expandVar v]
expandVarValor (VarList vs) =
  let expandedLists = map expandVarValor vs
      combinations = sequence expandedLists
  in [(ValorList (map fst comb), foldr andBDD ttPC (map snd comb)) | comb <- combinations, sat (foldr andBDD ttPC (map snd comb))]
expandVarValor (VarPair (v1, v2)) =
  let exp1 = expandVarValor v1
      exp2 = expandVarValor v2
  in [(ValorPair (val1, val2), andBDD pc1 pc2) | (val1, pc1) <- exp1, (val2, pc2) <- exp2, sat (andBDD pc1 pc2)]

toValorList :: VarValor -> [Valor]
toValorList var = map fst (expandVarValor var)
