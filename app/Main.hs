module Main where

import Data.List
import System.Environment
import System.IO
import System.Random
import Data.Char

sanitize :: String -> String
sanitize str = filter charsOnly $ map toLower str
  where
    charsOnly a = elem a ['a'..'z']

getWord :: [String] -> Int -> String
getWord words idx = sanitize (head (drop idx words)) ++ " "

main :: IO ()
main = do
  contents <- readFile "/usr/share/dict/words"
  let words = lines contents
  gen <- getStdGen
  let idxs = take 4 (randomRs (0, length words) gen)
  let out = map (getWord words) idxs
  mapM putStr out
  putStrLn ""
