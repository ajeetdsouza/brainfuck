module Ast where

data Node = Node { nodeType :: NodeType, lineNo :: Int, colNo :: Int }
  deriving Show

data NodeType = Shift Int
              | Add Int
              | Print
              | Read
              | Loop [Node]
  deriving Show
