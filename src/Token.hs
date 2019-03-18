module Token where

data Token = Token { tokenType :: TokenType  -- type of token
                   , lineNo :: Int  -- line
                   , colNo :: Int -- col
                   }
  deriving Show

data TokenType = ShiftRight         -- >
               | ShiftLeft          -- <
               | Increment          -- +
               | Decrement          -- -
               | Print              -- .
               | Read               -- ,
               | LoopL              -- [
               | LoopR              -- ]
  deriving Show
