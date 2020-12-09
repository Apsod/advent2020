module Day8 (solve) where

import Data.Either
import Data.Array
import Data.Array.ST
import Data.STRef
import Control.Monad.ST
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS


data Expression =
    Nop Int
  | Acc Int
  | Jmp Int
  deriving Show


type Program = Array Int Expression

dataparser :: Parser [Expression]
dataparser = (line_parser `sepBy` endOfLine) <* skipSpace <* endOfInput
  where
    num_parser = signed decimal
    nop_parser = Nop <$> ("nop " *> num_parser)
    acc_parser = Acc <$> ("acc " *> num_parser)
    jmp_parser = Jmp <$> ("jmp " *> num_parser)
    line_parser = choice [nop_parser, acc_parser, jmp_parser]


action :: Expression -> (Int, Int) -> (Int, Int)
action (Nop delta) (ptr, acc) = (ptr+1, acc)
action (Acc delta) (ptr, acc) = (ptr+1, acc+delta)
action (Jmp delta) (ptr, acc) = (ptr+delta, acc)

step :: Program -> (Int, Int) -> (Int, Int)
step program state@(ptr, _) = action (program ! ptr) state

run :: [Expression] -> Either Int Int
run expressions =
  let
    n = length expressions
    program = listArray (1, n) expressions
    execute = step program
    go :: forall s. (Int, Int) -> STUArray s Int Bool -> ST s (Either Int Int)
    go state@(ptr, acc) visited = case ptr > n of
      True -> return $ Right acc
      False -> do
        is_visited <- readArray visited ptr
        if is_visited then
          return $ Left acc
        else
          writeArray visited ptr True >> go (execute state) visited
  in runST (newArray (1, n) False >>= go (1, 0))


variants :: [Expression] -> [[Expression]]
variants (Nop d : xs) =
  let rest = variants xs
  in (Jmp d : xs) : (fmap (Nop d :) rest)
variants (Jmp d : xs) =
  let rest = variants xs
  in (Nop d : xs) : (fmap (Jmp d :) rest)
variants (Acc d : xs) = (Acc d :) <$> variants xs
variants [] = []



solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print $ run xs
  print . head . rights . fmap run $ variants xs





