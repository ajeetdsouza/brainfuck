module Parser where

import qualified Token
import qualified Ast
import qualified ParserError

parse :: [Token.Token] -> Either ParserError.ParserError [Ast.Node]
parse [] = Right []
parse (token:tokens) = do
  (node, rest) <- parseNode
  nodes <- parse rest
  return $ node:nodes
  where
    lineNo = Token.lineNo token

    colNo = Token.colNo token

    makeAstNode nodeType = Ast.Node
      { Ast.nodeType = nodeType
      , Ast.lineNo = lineNo
      , Ast.colNo = colNo
      }

    makeParserError parserErrorType = ParserError.ParserError
      { ParserError.parserErrorType = parserErrorType
      , ParserError.lineNo = lineNo
      , ParserError.colNo = colNo
      }

    parseNode = case Token.tokenType token of
      Token.ShiftLeft  -> do
        let node = makeAstNode $ Ast.Shift (-1)
        return (node, tokens)
      Token.ShiftRight -> do
        let node = makeAstNode $ Ast.Shift 1
        return (node, tokens)
      Token.Increment  -> Right (makeAstNode $ Ast.Add 1, tokens)
      Token.Decrement  -> Right (makeAstNode $ Ast.Add (-1), tokens)
      Token.Print      -> Right (makeAstNode Ast.Print, tokens)
      Token.Read       -> Right (makeAstNode Ast.Read, tokens)
      Token.LoopL      -> do
        (loop, rest) <- splitLoop 1 tokens
        parsedLoop <- parse loop
        let node = makeAstNode $ Ast.Loop parsedLoop
        return (node, rest)
      Token.LoopR      -> Left $ makeParserError ParserError.UnmatchedLoopR

    splitLoop 1 (Token.Token { Token.tokenType = Token.LoopR }:tokens) =
      Right ([], tokens)
    splitLoop n [] = Left $ makeParserError ParserError.UnmatchedLoopL
    splitLoop n (token:tokens) = case token of
      Token.Token { Token.tokenType = Token.LoopL } -> do
        (loop, rest) <- splitLoop (n + 1) tokens
        return (token:loop, rest)
      Token.Token { Token.tokenType = Token.LoopR } -> do
        (loop, rest) <- splitLoop (n - 1) tokens
        return (token:loop, rest)
      _ -> do
        (loop, rest) <- splitLoop n tokens
        return (token:loop, rest)
