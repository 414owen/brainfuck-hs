{-# LANGUAGE TupleSections #-}

module Interpret
  ( interpret
  ) where

import AST
import Data.Word

type Cell = Word8
type Tape = ([Cell], Cell, [Cell])

getCell :: IO Cell
getCell = fmap (toEnum . fromEnum) getChar

putCell :: Cell -> IO ()
putCell = putChar . (toEnum . fromEnum)

exec :: AST -> Tape -> IO ()
exec [] _                                     = pure ()
exec (IncPtr         : nast) (l, c, (r : rs)) = exec nast (c : l, r, rs)
exec (DecPtr         : nast) ((l : ls), c, r) = exec nast (ls, l, c : r)
exec (IncData        : nast) (l, c, r)        = exec nast (l, c + 1, r)
exec (DecData        : nast) (l, c, r)        = exec nast (l, c - 1, r)
exec (Output         : nast) tape@(l, c, r)   = putCell c >> exec nast tape
exec (Input          : nast) (l, _, r)        = getCell >>= exec nast . (l,,r)
exec (OpenCond goto  : nast) tape@(l, 0, r)   = exec goto tape
exec (OpenCond goto  : nast) tape             = exec nast tape
exec (CloseCond goto : nast) tape@(l, 0, r)   = exec nast tape
exec (CloseCond goto : nast) tape             = exec goto tape

interpret ast = exec ast (repeat 0, 0, repeat 0)
