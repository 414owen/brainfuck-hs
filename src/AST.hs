module AST
  ( AST (..)
  , ASTNode (..)
  ) where

newtype AST = AST [ASTNode]

data ASTNode =
    IncDataPtr
  | DecDataPtr
  | IncData
  | DecData
  | Output
  | Input
  | OpenCond AST
  | CloseCond AST

nodeToChar :: ASTNode -> Char
nodeToChar IncDataPtr    = '>'
nodeToChar DecDataPtr    = '<'
nodeToChar IncData       = '+'
nodeToChar DecData       = '-'
nodeToChar Output        = '.'
nodeToChar Input         = ','
nodeToChar (OpenCond  _) = '['
nodeToChar (CloseCond _) = ']'

instance Show ASTNode where
  show = pure . nodeToChar 

instance Show AST where
  show (AST nodes) = fmap nodeToChar nodes
