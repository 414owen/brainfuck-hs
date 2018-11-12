module Parse
  ( parse
  ) where

import AST
import Data.Bifunctor

simpleParse :: Char -> Maybe ASTNode
simpleParse '>' = Just IncPtr
simpleParse '<' = Just DecPtr
simpleParse '+' = Just IncData
simpleParse '-' = Just DecData
simpleParse '.' = Just Output
simpleParse ',' = Just Input
simpleParse _   = Nothing

parseRec :: [AST] -> String -> (AST, [AST])
parseRec _     []         = ([], [])
parseRec conds ('[' : xs) =
  let recced@(nodes, futureConds) = parseRec (nodes : conds) xs in
    case futureConds of
      (futCond : ys) -> (OpenCond futCond : nodes, ys)
      []             -> error "unmatched opening brace"
parseRec []     (']' : xs) = error "unmatched closing brace"
parseRec (y:ys) (']' : xs) =
  let recced@(nodes, futureConds) = parseRec ys xs in
    (CloseCond y : nodes, nodes : futureConds)
parseRec conds (x : xs) = let recced = parseRec conds xs in
  case simpleParse x of
    Nothing   -> recced
    Just node -> first (node:) recced

parse :: String -> AST
parse = fst . parseRec []
