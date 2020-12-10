module Day9 (solve) where

import Sliding 
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS
import Data.List (find)

dataparser :: Parser [Int]
dataparser = (decimal `sepBy` endOfLine) <* skipSpace <* endOfInput

data PairSum = PairSum IntSet IntSet
    deriving Show


instance Semigroup PairSum where
    (PairSum a aa) <> (PairSum b bb) = PairSum c cc
        where 
            c = IntSet.union a b
            cc = IntSet.unions [aa, bb, ab]
            ab = IntSet.fromList $ (+) <$> IntSet.toList a <*> IntSet.toList b

instance Monoid PairSum where
    mempty = PairSum IntSet.empty IntSet.empty

singleton :: Int -> PairSum
singleton x = PairSum (IntSet.singleton x) IntSet.empty

solve1 :: Int -> [Int] -> Maybe Int
solve1 window xs = fmap snd . find (\(PairSum _ set, x) -> not $ IntSet.member x set) $ drop window (zip sw $ tail xs)
    where sw = slidingWindow window $ fmap singleton xs

solve2 :: Int -> [Int] -> Int
solve2 target xs = go [] xs
    where 
        go sums (x:xs) = 
            let
                sums' = Prelude.takeWhile (\(acc, _, _) -> acc <= target) ((x, x, x) : fmap (\(acc, lb, ub) -> (acc+x, min x lb, max x ub)) sums)
            in case find (\(acc, _, _) -> acc==target) (tail sums') of
                    Just (_, lb, ub) -> lb + ub
                    Nothing -> go sums' xs

solve :: String -> IO ()
solve filepath = do
    Right xs <- parseOnly dataparser <$> BS.readFile filepath
    let Just solution1 = solve1 25 xs
    print solution1
    print $ solve2 solution1 xs

