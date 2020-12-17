module Day17(solve) where

import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS

import qualified Data.List as List

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

type State = Set [Int]

dataparser :: Parser [(Int, Int)]
dataparser = toCoord <$> (lineparser `sepBy` endOfLine)
    where 
        lineparser :: Parser [Int] 
        lineparser = List.elemIndices '#' . BS.unpack <$> P.takeWhile (inClass ".#")
        toCoord :: [[Int]] -> [(Int, Int)]
        toCoord rows = do 
            (rix, row) <- zip [0..] rows 
            cix <- row
            return (rix, cix)


mkState1 :: [(Int, Int)] -> State
mkState1 = Set.fromList . fmap (\(x, y) -> [x, y, 0])

mkState2 :: [(Int, Int)] -> State
mkState2 = Set.fromList . fmap (\(x, y) -> [x, y, 0, 0])

getAdjacent :: [Int] -> [[Int]]
getAdjacent = tail . go 
    where 
        go [] = [[]]
        go (x:rest) = do
            x' <- [x, x+1, x-1] 
            rest' <- go rest
            return (x':rest')

step :: State -> State
step set = 
    let count = Map.fromListWith (+) $ do
            point <- Set.toList set
            adjacent <- getAdjacent point
            return (adjacent, 1)
        active' coord count = (active && 2 <= count && count <= 3) || (not active && count == 3)
            where active = coord `Set.member` set
    in Map.keysSet $ Map.filterWithKey active' count


solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print . Set.size . (!!6)  . iterate step $ mkState1 xs 
  print . Set.size . (!!6)  . iterate step $ mkState2 xs 
