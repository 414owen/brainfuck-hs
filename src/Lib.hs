module Lib
    ( runBF
    ) where

import Parse
import Interpret

runBF :: String -> IO ()
runBF = interpret . parse
