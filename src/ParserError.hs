{-# LANGUAGE OverloadedStrings #-}

module ParserError where

import qualified Data.Text as T
import           Pretty

data ParserErrorType = UnmatchedLoopL
                     | UnmatchedLoopR
  deriving Show

data ParserError = ParserError { parserErrorType :: ParserErrorType
                               , lineNo :: Int
                               , colNo :: Int
                               }
  deriving Show

instance Pretty ParserError where
  pshow parserError = T.concat [message, location]
    where
      message = case parserErrorType parserError of
        UnmatchedLoopL -> "Unmatched ["
        UnmatchedLoopR -> "Unmatched ]"

      location = T.concat
        [ " at line "
        , T.pack . show $ lineNo parserError
        , ", col "
        , T.pack . show $ colNo parserError]
