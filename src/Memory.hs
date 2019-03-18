module Memory where

data Stream a = a :| Stream a deriving Show

data Tape a = Tape (Stream a) a (Stream a) deriving Show

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l :| ls) x rs) = Tape ls l (x :| rs)

moveRight :: Tape a -> Tape a
moveRight (Tape ls x (r :| rs)) = Tape (x :| ls) r rs

move :: Int -> Tape a -> Tape a
move n tape
  | n == 0 = tape
  | n < 0 = composeN (-n) moveLeft tape
  | n > 0 = composeN n moveRight tape
  where
    composeN n f = foldr (.) id $ replicate n f
