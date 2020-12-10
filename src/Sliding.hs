module Sliding where

import Data.Bits
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Sequence
import Data.Maybe (catMaybes)

type WindowState a = [ Seq a ]

log2 :: Int -> Int
log2 x = finiteBitSize x - 1 - countLeadingZeros x

initWindow :: Monoid a => Int -> WindowState a
initWindow levels = fmap mkLevel [0..levels]
    where mkLevel ix = Sequence.replicate (bit ix) mempty

pushWindow :: Monoid a => a -> WindowState a -> WindowState a
pushWindow _ [] = []
pushWindow x ((carry :<| left) : rest) = (left |> x) : pushWindow (carry <> x) rest

getIndices :: Int -> [Maybe Int]
getIndices n = fmap get [0..32]
    where
        get ix = if testBit n ix then Just ((bit ix - 1) .&. n) else Nothing

extractWindow :: Monoid a => Int -> WindowState a -> a
extractWindow n = mconcat . reverse . catMaybes . zipWith (\ix s -> getAt s <$> ix) (getIndices n)
    where getAt s ix = Sequence.index s $ Sequence.length s - ix - 1

slidingWindow :: Monoid a => Int -> [a] -> [a]
slidingWindow window = go (initWindow $ log2 window)
    where
        extract = extractWindow window 
        go _ [] = []
        go state (x:xs) = extract state' : go state' xs 
            where state' = pushWindow x state
