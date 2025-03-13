-- BenchmarkCriterion.hs (updated to use Criterion 'bench' and save JSON report)

module BenchmarkCriterion where

import Criterion.Main
import Criterion.Types (Benchmarkable(..))
import Criterion.Measurement (initializeTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName)
import System.IO
import Control.Exception (evaluate)
import Control.DeepSeq (rnf)
import Data.Time.Clock (getCurrentTime)

import Benchmark
import WhileLang.WhileEncoder
import ComplexExamples
import qualified Data.Map as Map
import Base.Types (Valor(..))
import Variability.VarTypes (VarValor(..), Var(..), Val, PresenceCondition, sat, andBDD, valList, ttPC)
import Memoization.Core.Memory (KeyValueArray, FuncKey(..), resetCounters, sumCounters)
import Memoization.Core.State (State(..))

-- Versioned programs (v1, v2)
programs :: [(String, Program, Program)]
programs =
  [ ("Deep Loop", deep_loop, deep_loop_v2)
  , ("Nested Loop", nested_variability, nested_variability_v2)
  , ("Interprocedural", interprocedural_sim, interprocedural_sim_v2)
  , ("Recursion Sim", factorial_rec_sim, factorial_rec_sim_v2)
  , ("Arithmetic Heavy", arithmetic_heavy, arithmetic_heavy_v2)
  , ("Variational Init", init_variability, init_variability_v2)
  , ("Multi Variant Loop", loop_multi_variant, loop_multi_variant_v2)
  , ("Deep Nested Variants", deep_nested_variants, deep_nested_variants_v2)
  ]

interpreters :: [Interpreter]
interpreters = [Base, Memoized, Variational, VMemoized]

memEmptyBase :: Either (MemState Valor) (MemState VarValor)
memEmptyBase = Left (MemState [])

memEmptyVar :: Either (MemState Valor) (MemState VarValor)
memEmptyVar = Right (MemState [])

benchmarkAll :: IO ()
benchmarkAll = do
  createDirectoryIfMissing True outputDir
  initializeTime
  allBenchmarks <- fmap concat $ mapM (\analysis@(analysisPath, memoizedFunctions) -> do
    let analysisLabel = takeBaseName analysisPath
    concat <$> mapM (\interpreter -> do
        mapM (\(progLabel, progV1, progV2) -> do
          let label = analysisLabel ++ "_" ++ progLabel

          -- Bench V1
          memStateV1 <- case interpreter of
            Base        -> return memEmptyBase
            Memoized    -> return $ Left (MemState [])
            Variational -> return memEmptyVar
            VMemoized   -> return $ Right (MemState [])

          let benchV1Name = show interpreter ++ "_v1: " ++ analysisLabel ++ "/" ++ progLabel
          let benchActionV1 = do
                (result, finalMemState) <- runAnalysis interpreter (analysisPath, memoizedFunctions) progV1 memStateV1
                evaluate (rnf result)
                case (interpreter, finalMemState) of
                  (Memoized, Left m) -> saveMemoryState Memoized (label ++ "_v1") m
                  (VMemoized, Right m) -> saveMemoryState VMemoized (label ++ "_v1") m
                  _ -> return ()

          -- Bench V2
          memStateV2 <- case interpreter of
            Memoized   -> Left <$> loadMemoryState Memoized (label ++ "_v1") True
            VMemoized  -> Right <$> loadMemoryState VMemoized (label ++ "_v1") True
            Base       -> return memEmptyBase
            Variational -> return memEmptyVar

          let benchV2Name = show interpreter ++ "_v2: " ++ analysisLabel ++ "/" ++ progLabel
          let benchActionV2 = do
                (result, finalMemState) <- runAnalysis interpreter (analysisPath, memoizedFunctions) progV2 memStateV2
                evaluate (rnf result)
                case (interpreter, finalMemState) of
                  (Memoized, Left m) -> saveMemoryState Memoized (label ++ "_v2") m
                  (VMemoized, Right m) -> saveMemoryState VMemoized (label ++ "_v2") m
                  _ -> return ()

          return [ bench benchV1Name (nfIO benchActionV1)
                , bench benchV2Name (nfIO benchActionV2)
                ]
          ) programs
      ) interpreters
    ) analyses
  defaultMain (concat allBenchmarks)
