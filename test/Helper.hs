module Helper where

import Paths_lifting_framework (getDataFileName)
import qualified Data.Text as T

processFile :: (String -> a -> b) -> String -> a -> IO b
processFile calc path input = do
  filePath <- getDataFileName path
  content <- readFile filePath
  return (calc content input)

replaceString :: String -> String -> String -> String
replaceString old new text = T.unpack (T.replace (T.pack old) (T.pack new) (T.pack text))

substitute :: String -> [(String, String)] -> String
substitute text substitutions =
    let replaceAll :: [(String, String)] -> String -> String
        replaceAll [] txt = txt
        replaceAll ((key, value):xs) txt =
            replaceAll xs (replaceString key value txt)
    
    in replaceAll substitutions text