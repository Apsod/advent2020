module Day2 (solve) where

import Data.Monoid
import Data.Bool
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS

dataparser ::  Parser [(Int, Int, Char, BS.ByteString)]
dataparser = manyTill lineparser endOfInput
  where
    lineparser = (,,,) <$>
      (int <* "-") <*>
      (int <* " ") <*>
      (anyChar <* ": ") <*>
      (takeTill isSpace <* skipSpace)
    int = fromIntegral <$> decimal

valid_1 :: (Int, Int, Char, BS.ByteString) -> Bool
valid_1 (lb, ub, char, password) = ok . BS.length $ BS.filter(==char) password
  where ok x = x >= lb &&  x <= ub

valid_2 :: (Int, Int, Char, BS.ByteString) -> Bool
valid_2 (ix1, ix2, char, password) = 
  let 
    c1 = BS.index password (ix1+1)
    c2 = BS.index password (ix2+1)
  in c1 == char || c2 == char

check :: (Int, Int, Char, BS.ByteString) -> (Sum Int, Sum Int)
check (a, b, char, password) =
  let
    indices = BS.elemIndices char password
    f1 = (length indices >= a && length indices <= b)
    f2 = (==1) . length $ filter (\x -> x+1 == a || x+1 == b) indices
    bool2sum = Sum . bool 0 1
  in (bool2sum f1, bool2sum f2)

boolCount :: Bool -> Sum Int
boolCount = Sum . bool 0 1

solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print $ foldMap check xs
