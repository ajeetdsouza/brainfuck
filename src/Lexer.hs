module Lexer where

import           Control.Monad
import           Data.Maybe
import           Token

lex :: [String] -> [Token]
lex = concat . zipWith lexLine [1 ..]
  where
    lexLine lineNo = catMaybes . zipWith (lexCol lineNo) [1 ..]

    lexCol lineNo colNo char = case lexChar char of
      Nothing        -> Nothing
      Just tokenType -> Just
        $ Token { tokenType = tokenType, lineNo = lineNo, colNo = colNo }

    lexChar char = case char of
      '>' -> Just ShiftRight
      '<' -> Just ShiftLeft
      '+' -> Just Increment
      '-' -> Just Decrement
      '.' -> Just Print
      ',' -> Just Read
      '[' -> Just LoopL
      ']' -> Just LoopR
      _   -> Nothing -- ignore comments
