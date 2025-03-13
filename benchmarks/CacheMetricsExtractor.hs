-- CacheMetricsExtractor.hs (enhanced for v1/v2-aware cache metric extraction)

module Main where

import Benchmark (Interpreter(..), MemState(..), memoryFileName, loadMemoryState)
import Memoization.Core.Memory (sumCounters)
import Memoization.Core.State (runState)
import System.Directory (listDirectory, doesFileExist, getFileSize)
import System.FilePath (takeBaseName, takeExtension, dropExtension, splitExtension)
import System.IO
import Data.List (isPrefixOf, isSuffixOf, stripPrefix)
import Base.Types (Valor(..))
import Variability.VarTypes (VarValor(..), Var(..), Val, PresenceCondition, sat, andBDD, valList, ttPC)
import Debug.Trace

memEmptyBase :: Either (MemState Valor) (MemState VarValor)
memEmptyBase = Left (MemState [])

memEmptyVar :: Either (MemState Valor) (MemState VarValor)
memEmptyVar = Right (MemState [])

main :: IO ()
main = do
  let interpreters = [Memoized, VMemoized]
  let outputFile = "benchmark_output/cache_metrics.csv"
  writeFile outputFile "Interpreter,Analysis,Program,Version,CacheSize,CacheMiss,CacheHits\n"
  mapM_ (processInterpreter outputFile) interpreters
  putStrLn "Cache metrics (v1/v2) extracted into benchmark_output/cache_metrics.csv"

processInterpreter :: FilePath -> Interpreter -> IO ()
processInterpreter outputFile interpreter = do
  files <- listDirectory "benchmark_output"
  let relevantFiles = filter (matchesInterpreter interpreter) files
  mapM_ (processFile outputFile interpreter) relevantFiles

matchesInterpreter :: Interpreter -> FilePath -> Bool
matchesInterpreter Memoized file = takeExtension file == ".dat" && "memoized_state_" `isPrefixOf` file
matchesInterpreter VMemoized file = takeExtension file == ".dat" && "vmemoized_state_" `isPrefixOf` file
matchesInterpreter _ _ = False

processFile :: FilePath -> Interpreter -> FilePath -> IO ()
processFile outputFile interpreter file = do
  cacheSize <- getCacheFileSize ("benchmark_output/" ++ file)
  let (labelRaw, _) = splitExtension file
  let version = if "_v2" `isSuffixOf` labelRaw then "v2"
                else if "_v1" `isSuffixOf` labelRaw then "v1"
                else ""
  let label = case stripPrefix (prefix interpreter) labelRaw of
              Just stripped -> stripped
              Nothing -> labelRaw
  let labelWithoutVersion = reverse (drop 3 (reverse label))
  let (analysis, prog) = splitLabel labelWithoutVersion
  mem <- case interpreter of
            Memoized   -> Left <$> loadMemoryState Memoized label False
            VMemoized  -> Right <$> loadMemoryState VMemoized label False
            Base       -> return memEmptyBase
            Variational -> return memEmptyVar
  let (cacheMiss, cacheHits) = case mem of
        Left m  -> getCacheStats m
        Right m -> getCacheStats m
  appendFile outputFile $ analysis ++ "," ++ prog ++ "," ++ version ++ "," ++ show interpreter ++ "," ++ show cacheSize ++ "," ++ show cacheMiss ++ "," ++ show cacheHits ++ "\n"

splitLabel :: String -> (String, String)
splitLabel str =
  case break (== '_') str of
    (a, '_' : b) -> (a, b)
    _            -> (str, "")

prefix :: Interpreter -> String
prefix Memoized  = "memoized_state_"
prefix VMemoized = "vmemoized_state_"
prefix _         = ""

getCacheStats :: MemState v -> (Int, Int)
getCacheStats (MemState memState) =
  let cacheMiss = length memState
      cacheHits = fst (sumCounters `runState` memState)
  in (cacheMiss, cacheHits)

getCacheFileSize :: String -> IO Integer
getCacheFileSize file = do
    exists <- doesFileExist file
    if exists then getFileSize file else return 0
