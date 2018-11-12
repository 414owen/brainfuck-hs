{-# LANGUAGE TupleSections #-}

module Interpret
  ( interpret
  ) where

import AST

type Tape = ([Char], Char, [Char])

exec :: AST -> Tape -> IO ()
exec [] _                                     = pure ()
exec (IncPtr       : nast) (l, c, (r : rs))   = exec nast (c : l, r, rs)
exec (DecPtr       : nast) ((l : ls), c, r)   = exec nast (ls, l, c : r)
exec (IncData          : nast) (l, c, r)      = exec nast (l, succ c, r)
exec (DecData          : nast) (l, c, r)      = exec nast (l, pred c, r)
exec (Output           : nast) tape@(l, c, r) = putChar c >> exec nast tape
exec (Input            : nast) (l, _, r)      = getChar >>= exec nast . (l,,r)
exec (OpenCond goto  : nast) tape@(l, '\0', r)   = exec goto tape
exec (OpenCond goto  : nast) tape             = exec nast tape
exec (CloseCond goto : nast) tape@(l, '\0', r)   = exec nast tape
exec (CloseCond goto : nast) tape             = exec goto tape

interpret ast = exec ast (repeat '\0', '\0', repeat '\0')
