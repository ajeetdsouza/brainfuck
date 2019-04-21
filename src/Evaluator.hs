module Evaluator where

import qualified Data.Char as Char
import           System.IO
import qualified Ast
import qualified Memory

emptyTape :: Memory.Tape Int
emptyTape = Memory.Tape stream 0 stream
  where
    stream = (Memory.:|) 0 stream

eval :: Memory.Tape Int -> [Ast.Node] -> IO (Memory.Tape Int)
eval mem [] = return mem
eval mem @ (Memory.Tape ml mx mr) prg @ (node:nodes) = case Ast.nodeType node of
  Ast.Shift n   -> eval (Memory.move n mem) nodes
  Ast.Add x     -> let mx' = (mx + x) `mod` 256
                   in eval (Memory.Tape ml mx' mr) nodes
  Ast.Print     -> do
    putChar . Char.chr $ mx
    eval mem nodes
  Ast.Read      -> do
    mx' <- Char.ord <$> getChar
    eval (Memory.Tape ml mx' mr) nodes
  Ast.Loop loop ->
    if mx == 0
      then eval mem nodes
      else do
        mem' <- eval mem loop
        eval mem' prg
