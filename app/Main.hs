module Main where

import System.Directory
import System.Environment
import System.Directory.Tree
import Data.List

type Extension = String

filterByExt :: [Extension] -> DirTree a -> Bool
filterByExt exts (Dir _ _) = True
filterByExt exts (File name _) = any (`isSuffixOf` name) exts
filterByExt exts (Failed _ _) = False

removeDirEntries :: DirTree a -> Bool
removeDirEntries (Dir _ _) = False
removeDirEntries (File _ _) = True

toPath :: DirTree a -> a
toPath (File _ path) = path

stripEmptyLines :: [String] -> [String]
stripEmptyLines = filter (not . null)

processLOC :: String -> IO (Int, Int)
processLOC path = readFile path >>=
  return . (\x -> (allLoc x, strictLOC x))
    where allLoc = length . lines
          strictLOC = length . stripEmptyLines . lines

main :: IO ()
main = do
  extensions <- getArgs
  cwd <- getCurrentDirectory
  dirStructure <- build cwd
  let dTree = dirTree dirStructure
  let transformed = flattenDir $ filterDir (filterByExt extensions) dTree
  let files = filter removeDirEntries transformed
  let paths = map toPath files
  processed <- mapM processLOC paths
  let strictLoc = sum $ map snd processed
  let allLoc = sum $ map fst processed
  print ("Lines: " <> show allLoc)
  print ("LOC strict: " <> show strictLoc)
