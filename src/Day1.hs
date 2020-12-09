module Day1 (solve) where

import Data.Attoparsec.ByteString.Char8 as P
import Data.IntMap as M
import qualified Data.ByteString.Char8 as BS

dataparser :: P.Parser [Int]
dataparser = manyTill (int <* skipSpace) endOfInput
  where int = fromIntegral <$> decimal

find :: [Int] -> (Int, Int)
find xs = go xs empty empty empty
  where
    add :: Int -> IntMap Int -> IntMap Int
    add x =
      M.map (*x)
      . M.filterWithKey (\key _ -> key <= 2020)
      . M.mapKeysMonotonic (+x)

    go :: [Int] -> IntMap Int -> IntMap Int -> IntMap Int -> (Int, Int)
    go [] c1 c2 c3 = (c2 ! 2020, c3 ! 2020)
    go (x:xs) c1 c2 c3 =
      let
        c1' = M.insert x x c1
        c2' =
          M.union c2
          $ add x c1
        c3' = 
          M.union c3
          $ add x c2
      in go xs c1' c2' c3'

solve :: String -> IO ()
solve filepath = do
  xs <- parseOnly dataparser <$> BS.readFile filepath
  either print (print . find) xs
