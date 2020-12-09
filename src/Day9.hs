module Day9 (solve) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Sequence
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Data.Bits
import Data.Maybe (catMaybes)

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

type WindowState a = [ Seq a ]

initWindow :: Monoid a => Int -> WindowState a
initWindow max = go 1
    where go n = case compare n max of
            EQ -> [Sequence.singleton mempty]
            LT -> Sequence.replicate n mempty : go (n*2)
            GT -> []

pushWindow :: Monoid a => a -> WindowState a -> WindowState a
pushWindow _ [] = []
pushWindow x ((carry :<| left) : rest) = (left |> x) : pushWindow (carry <> x) rest

getIndices :: Int -> [Maybe Int]
getIndices n = fmap get [0..10]
    where
        get ix = if testBit n ix then Just ((bit ix - 1) .&. n) else Nothing

extractWindow :: Monoid a => Int -> WindowState a -> a
extractWindow n = mconcat . reverse . catMaybes . zipWith (\ix s -> getAt s <$> ix) (getIndices n)
    where getAt s ix = Sequence.index s $ Sequence.length s - ix - 1

slidingWindow :: Monoid a => Int -> [a] -> [a]
slidingWindow window = go (initWindow window)
    where
        extract = extractWindow window 
        go _ [] = []
        go state (x:xs) = extract state' : go state' xs 
            where state' = pushWindow x state

solve1 :: Int -> [Int] -> Maybe Int
solve1 window xs = fmap snd . find (\(PairSum _ set, x) -> not $ IntSet.member x set) $ drop window (zip sw $ tail xs)
    where sw = slidingWindow window $ fmap singleton xs

solve2 :: Int -> [Int] -> Int
solve2 target xs = go [] xs
    where 
        go sums (x:xs) = 
            let
                sums' = Prelude.takeWhile (\(acc, lb, ub) -> acc <= target) ((x, x, x) : fmap (\(acc, lb, ub) -> (acc+x, min x lb, max x ub)) sums)
            in case find (\(acc, _, _) -> acc==target) (tail sums') of
                    Just (_, lb, ub) -> lb + ub
                    Nothing -> go sums' xs

solve :: String -> IO ()
solve filepath = do
    Right xs <- parseOnly dataparser <$> BS.readFile filepath
    let Just solution1 = solve1 25 xs
    print solution1
    print $ solve2 solution1 xs

