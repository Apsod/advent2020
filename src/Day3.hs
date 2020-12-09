module Day3 (solve) where

import Data.Attoparsec.ByteString.Char8 as P
import Data.Array
import Data.Bool
import qualified Data.ByteString.Char8 as BS

dataparser :: P.Parser (Array (Int, Int) Char)
dataparser = toArray <$> manyTill parseline endOfInput
  where
    parseline = BS.unpack <$> (takeTill isSpace <* skipSpace)
    toArray bss =
      let
        w = length $ head bss
        h = length bss
      in listArray ((0,0), (h-1, w-1)) (bss >>= id)

count_hits :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Int
count_hits map = go
  where
    (_, (h, w)) = bounds map
    go (y, x) (dy, dx) =
      let 
        x' = (x + dx) `mod` (w+1)
        y' = y + dy
        hit = bool 0 1 $ (map ! (y, x)) == '#'
      in case y > h of
        True -> 0
        False -> hit + go (y', x') (dy, dx)


solve1 :: Array (Int, Int) Char -> Int
solve1 map = count_hits map (0, 0) (1, 3)

solve2 :: Array (Int, Int) Char -> Int
solve2 map = go (1, 1) * go (1, 3) * go (1, 5) * go (1, 7) * go (2, 1)
  where go = count_hits map (0, 0)

solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print $ solve1 xs
  print $ solve2 xs
