module Lib
    ( runBF
    ) where

import Parse

runBF :: IO ()
runBF =
  getContents >>=
  print . parse
