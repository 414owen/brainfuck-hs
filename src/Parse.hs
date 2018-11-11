module Parse where

import AST
import Data.Bifunctor

parse :: String -> AST
parse = fst . parseRec []
  where
    parseRec :: [AST] -> String -> (AST, [AST])
    parseRec _     []         = (AST [], [])
    parseRec conds ('>' : xs) = addNode IncDataPtr $ parseRec conds xs
    parseRec conds ('<' : xs) = addNode DecDataPtr $ parseRec conds xs
    parseRec conds ('+' : xs) = addNode IncData    $ parseRec conds xs
    parseRec conds ('-' : xs) = addNode DecData    $ parseRec conds xs
    parseRec conds ('.' : xs) = addNode Output     $ parseRec conds xs
    parseRec conds (',' : xs) = addNode Input      $ parseRec conds xs
    parseRec conds ('[' : xs) =
      let recced@(AST nodes, futureConds) = parseRec (AST nodes : conds) xs in
        case futureConds of
          (futCond : ys) -> (AST (OpenCond futCond : nodes), ys)
          []             -> error "unmatched opening brace"
    parseRec []     (']' : xs) = error "unmatched closing brace"
    parseRec (y:ys) (']' : xs) =
      let recced@(AST nodes, futureConds) = parseRec ys xs in
        (AST (CloseCond y : nodes), AST nodes: futureConds)
    addNode :: ASTNode -> (AST, [AST]) -> (AST, [AST])
    addNode node (AST nodes, gotos) = (AST (node : nodes), gotos)
