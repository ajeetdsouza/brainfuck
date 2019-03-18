module Main where

import           System.Environment
import           System.IO
import qualified Lexer
import qualified Parser
import qualified Evaluator
import qualified Optimizer
import           Pretty

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      handle <- openFile filePath ReadMode
      contents <- hGetContents handle
      let tokens = Lexer.lex . lines $ contents
      case Parser.parse tokens of
        Left err  -> pprint err
        Right ast -> do
          Evaluator.eval Evaluator.emptyTape (Optimizer.optimize ast)
          return ()
      hClose handle
    _          -> putStrLn "usage: ./brainfuck file.bf"
