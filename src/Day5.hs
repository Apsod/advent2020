module Day5 (solve) where

import Data.Bifunctor
import Data.List
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS

boolLookup :: (Eq a) => a -> a -> a -> Bool
boolLookup a b = f
  where f c = if c == a then False else if c == b then True else undefined

dataparser :: P.Parser [(Int, Int)]
dataparser = parseline `sepBy` skipSpace
  where
    parseline = (\r c -> (get_row r, get_col c)) <$> P.take 7 <*> P.take 3
    get_row = binaryParse $ boolLookup 'F' 'B'
    get_col = binaryParse $ boolLookup 'L' 'R'

binaryParse :: (Char -> Bool) -> BS.ByteString -> Int
binaryParse cmap = BS.foldl f 0
  where
    f acc x = case cmap x of
      True -> acc * 2 + 1
      False -> acc * 2

get_id :: (Int, Int) -> Int
get_id (r, c) = r * 8 + c

solutions :: [(Int, Int)] -> (Int, Int)
solutions xs =
  let
    ids = sort $ map get_id xs
    solution1 = last ids
    [solution2] = findHole ids
      where findHole ids = fmap ((+1) . fst) . filter (\(l, h) -> h - l /= 1) $ zip ids (tail ids)
  in (solution1, solution2)

solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print . solutions $ xs
