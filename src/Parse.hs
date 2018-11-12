module Parse where

import AST
import Data.Bifunctor

parse :: String -> AST
parse = fst . parseRec []
  where
    parseRec :: [AST] -> String -> (AST, [AST])
    parseRec _     []         = ([], [])
    parseRec conds ('>' : xs) = addNode IncPtr  $ parseRec conds xs
    parseRec conds ('<' : xs) = addNode DecPtr  $ parseRec conds xs
    parseRec conds ('+' : xs) = addNode IncData $ parseRec conds xs
    parseRec conds ('-' : xs) = addNode DecData $ parseRec conds xs
    parseRec conds ('.' : xs) = addNode Output  $ parseRec conds xs
    parseRec conds (',' : xs) = addNode Input   $ parseRec conds xs
    parseRec conds ('[' : xs) =
      let recced@(nodes, futureConds) = parseRec (nodes : conds) xs in
        case futureConds of
          (futCond : ys) -> (OpenCond futCond : nodes, ys)
          []             -> error "unmatched opening brace"
    parseRec []     (']' : xs) = error "unmatched closing brace"
    parseRec (y:ys) (']' : xs) =
      let recced@(nodes, futureConds) = parseRec ys xs in
        (CloseCond y : nodes, nodes : futureConds)
    parseRec conds (_ : xs) = parseRec conds xs

    addNode :: ASTNode -> (AST, [AST]) -> (AST, [AST])
    addNode node (nodes, gotos) = (node : nodes, gotos)
