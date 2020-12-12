module Day10 (solve) where

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap
import Data.List as List
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS

dataparser ::  Parser [Int]
dataparser = (decimal `sepBy` endOfLine) <* skipSpace <* endOfInput

tailZipWith :: (a -> a -> b) -> [a] -> [b]
tailZipWith f xs@(_:xs') = zipWith f xs xs'

counter :: [Int] -> IntMap Int
counter = IntMap.fromListWith (+) . fmap (,1)

solve1 :: [Int] -> Int
solve1 = (\c -> (c ! 1) * (c ! 3)) . counter . (3:) . tailZipWith (flip (-)) . (0:) . sort

solve2 :: [Int] -> Int
solve2 = go (1 : [0,0..]) . (0:) . sort 
    where 
        go (p : _) [x] = p
        go (p : ps) (x : xs) =
            let 
                jumps = length $ List.takeWhile (<= x+3) xs
                (l, r) = splitAt jumps ps
                ps' = fmap (+p) l ++ r
            in go ps' xs


solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print $ solve1 xs
  print $ solve2 xs 
