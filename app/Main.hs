module Main where

import           Data.Char
import           Data.List
import           System.Clipboard
import           System.Environment
import           System.IO
import           System.Random

sanitize :: String -> String
sanitize str = filter charsOnly $ map toLower str
  where
    charsOnly a = elem a ['a' .. 'z']

getWord :: [String] -> Int -> String
getWord words idx = sanitize (head (drop idx words)) ++ " "

main :: IO ()
main = do
  contents <- readFile "/usr/share/dict/words"
  let words = lines contents
  gen <- getStdGen
  let idxs = take 4 (randomRs (0, length words) gen)
  let sentence = map (getWord words) idxs
  let out = concat sentence
  putStr out
  putStrLn ""
  setClipboardString out
