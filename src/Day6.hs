module Day6 (solve) where

import Data.Set as S
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS

dataparser :: Parser [[Set Char]]
dataparser = (groupparser `sepBy` (endOfLine <* endOfLine)) <* skipSpace <* endOfInput
  where
    ansparser = S.fromList . BS.unpack <$> (P.takeWhile1  (inClass "a-z"))
    groupparser = ansparser `sepBy` endOfLine

solve1 :: [[ Set Char]] -> Int
solve1 = sum . fmap (S.size . S.unions)

solve2 :: [[ Set Char]] -> Int
solve2 = sum . fmap (S.size . foldl1 S.intersection)

solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print . solve1 $ xs
  print . solve2 $ xs
