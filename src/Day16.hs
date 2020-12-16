module Day16(solve) where

import Control.Monad

import Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (transpose)

import Data.Set (Set, (\\))
import qualified Data.Set as Set

import Data.IntMap (IntMap, (!), (!?))
import qualified Data.IntMap as IntMap

data Restriction = Restriction {getName :: ByteString, getRanges :: [(Int, Int)]}
    deriving Show

type Ticket = [Int]

type Data = ([Restriction], Ticket, [Ticket])

dataparser :: Parser Data
dataparser = do
    restrictions <- restrictionparser `sepBy1` endOfLine

    "\n\nyour ticket:\n"
    ticket <- ticketparser

    "\n\nnearby tickets:\n"
    others <- ticketparser `sepBy1` endOfLine

    return (restrictions, ticket, others)
        where
            restrictionparser = Restriction <$> (P.takeWhile (/= ':') <* ": ") <*> (rangeparser `sepBy` " or ")
            rangeparser = (,) <$> (decimal <* "-") <*> decimal
            ticketparser = decimal `sepBy1` char ','


satisfies :: Int -> Restriction -> Bool
satisfies value = any (\(lb, ub) -> lb <= value && value <= ub) . getRanges


solve1 :: Data -> [Int]
solve1 (restrictions, _, nearby) = do
    ticket <- nearby
    field <- ticket
    guard $ any (field `satisfies`) restrictions
    [field]

getValid :: Data -> Data
getValid (restrictions, my, nearby) = (restrictions, my, nearby')
    where nearby' = filter (all (\field -> any (field `satisfies`) restrictions)) nearby


solve2 :: Data -> Int
solve2 (restrictions, my, nearby) = product . fmap (my !!) $ departure_ixs
    where 
        matching = getMatching . Set.fromList $ do
            (field, values) <- zip [0..] $ transpose (my : nearby)
            restriction <- restrictions
            guard $ all (`satisfies` restriction) values
            [(field, getName restriction)]

        departure_ixs = fmap fst . filter (("departure" `BS.isPrefixOf`) . snd) $ matching


getMatching :: Set (Int, ByteString) -> [(Int, ByteString)]
getMatching set = head $ go (Set.toList . Set.map fst $ set) Set.empty
    where
        ix2names :: IntMap (Set ByteString)
        ix2names = IntMap.fromListWith Set.union . fmap (\(i,n) -> (i, Set.singleton n)) . Set.toList $ set
        go :: [Int] -> Set ByteString -> [[(Int, ByteString)]]
        go [] _ = [[]]
        go (i:is) taken = do
            candidate <- Set.toList $ (ix2names ! i) \\ taken
            solution <- go is (Set.insert candidate taken)
            return ((i, candidate) : solution)





solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print . sum . solve1 $ xs
  print . solve2 . getValid $ xs

