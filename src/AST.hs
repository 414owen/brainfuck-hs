module AST where

type AST = [ASTNode]

data ASTNode =
    IncPtr
  | DecPtr
  | IncData
  | DecData
  | Output
  | Input
  | OpenCond AST
  | CloseCond AST

nodeToChar :: ASTNode -> Char
nodeToChar IncPtr    = '>'
nodeToChar DecPtr    = '<'
nodeToChar IncData       = '+'
nodeToChar DecData       = '-'
nodeToChar Output        = '.'
nodeToChar Input         = ','
nodeToChar (OpenCond  _) = '['
nodeToChar (CloseCond _) = ']'

instance Show ASTNode where
  show = pure . nodeToChar 

astToStr :: AST -> String
astToStr nodes = fmap nodeToChar nodes
