module Optimizer where

import qualified Ast

optimize :: [Ast.Node] -> [Ast.Node]
optimize [] = []
optimize (node:nodes) = case Ast.nodeType node of
  Ast.Shift k -> case nodes' of
    (Ast.Node { Ast.nodeType = Ast.Shift k' }:tailNodes')
      -> if k + k' == 0
         then tailNodes'
         else let node' = makeAstNode $ Ast.Shift (k + k')
              in node':tailNodes'
    _ -> node:nodes'
  Ast.Add k   -> case nodes' of
    (Ast.Node { Ast.nodeType = Ast.Add k' }:tailNodes')
      -> if k + k' == 0
         then tailNodes'
         else let node' = makeAstNode $ Ast.Add (k + k')
              in node':tailNodes'
    _ -> node:nodes'
  _           -> node:nodes'
  where
    nodes' = optimize nodes

    makeAstNode nodeType = Ast.Node
      { Ast.nodeType = nodeType
      , Ast.lineNo = Ast.lineNo node
      , Ast.colNo = Ast.colNo node
      }
