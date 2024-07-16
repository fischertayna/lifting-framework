module Helper where

import Paths_lifting_framework (getDataFileName)

processFile :: (String -> a -> b) -> String -> a -> IO b
processFile calc path input = do
  filePath <- getDataFileName path
  content <- readFile filePath
  return (calc content input)