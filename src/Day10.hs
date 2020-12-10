module Day10 (solve) where

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap
import Data.List as List
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS

dataparser ::  Parser [Int]
dataparser = (decimal `sepBy` endOfLine) <* skipSpace <* endOfInput


counter :: [Int] -> IntMap Int
counter = IntMap.fromListWith (+) . fmap (,1)

solve1 :: [Int] -> Int
solve1 xs = (ret ! 1) * (ret ! 3)
    where 
        xs' = 0 : sort xs
        ret = counter $ 3 : zipWith (-) (tail xs') xs'


solve2 :: [Int] -> Int
solve2 xs = go xs' (1 : [0,0..])
    where 
        xs' = 0 : sort xs
        go [x] (p : _) = p
        go (x : xs) (p : ps) =
            let 
                jumps = length $ List.takeWhile (<= x+3) xs
                (l, r) = splitAt jumps ps
                ps' = fmap (+p) l ++ r
            in go xs ps'


solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print $ solve1 xs
  print $ solve2 xs 
