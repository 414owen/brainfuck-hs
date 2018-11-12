module Main where

import Lib
import Data.Monoid
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = getArgs >>= \args ->
  case args of
    [] -> getProgName >>= \name ->
      hPutStrLn stderr ("Usage: " <> name <> " <path>") >>
      exitWith (ExitFailure 1)
    (fname : _) -> readFile fname >>= runBF
