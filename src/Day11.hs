module Day11 (solve) where

import Data.Ix
import Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HashMap
import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List as List


type Pos = (Int, Int)
type State a = HashMap Pos a

dataparser ::  Parser (State Char, (Pos, Pos))
dataparser = (\x -> (toData x, get_bounds x)) <$> ((P.takeWhile1 (inClass "L.") `sepBy` endOfLine) <* skipSpace <* endOfInput)
    where 
        get_bounds rows = ((0, 0), (length rows, BS.length $ head rows))
        toData :: [BS.ByteString] -> HashMap (Int, Int) Char
        toData rows = HashMap.fromList $ do
            (row_ix, row) <- zip [0..] rows
            (col_ix, elem) <- zip [0..] $ BS.unpack row
            case elem of
                'L' -> [((row_ix, col_ix), elem)]
                '.' -> []


step :: State Char -> State Char
step state = HashMap.mapWithKey update state
    where 
        update ix 'L' = if adj ix == 0 then '#' else 'L'
        update ix '#' = if adj ix >= 5 then 'L' else '#'
        adj :: (Int, Int) -> Int
        adj (r, c) = length . filter (=='#') $ do
            dr <- [-1, 0, 1]
            dc <- [-1, 0, 1]
            maybeToList $ HashMap.lookup (r + dr, c+dc) state

step2 :: State [(Int, Int)]-> State Char -> State Char
step2 visible = step' 
    where 
        step' state = HashMap.mapWithKey update state
            where 
                update ix 'L' = if adj ix == 0 then '#' else 'L'
                update ix '#' = if adj ix >= 6 then 'L' else '#'
                adj :: (Int, Int) -> Int
                adj ix = length . filter (=='#') $ do
                    adjacent <- visible ! ix
                    return (state ! adjacent)


visible :: State a -> (Pos, Pos) -> State [Pos]
visible state bounds = HashMap.mapWithKey (\k _ -> visible_from k) state
    where 
        get_first :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
        get_first (dr, dc) (r, c) = List.find (`HashMap.member` state) . List.takeWhile (inRange bounds) . fmap (\d -> (r+dr*d, c+dc*d)) $ [1..]
        visible_from ix = do
            dr <- [-1, 0, 1]
            dc <- [-1, 0, 1]
            maybeToList $ get_first (dr, dc) ix 

stabilize :: Eq a => (a -> a) -> a -> a
stabilize f x = if x' == x then x else stabilize f x'
    where x' = f x
    
solve1 :: State Char -> Int
solve1 = length . filter (=='#') . HashMap.elems . stabilize step

solve2 :: State Char -> (Pos, Pos) -> Int
solve2 state bounds = length . filter (=='#') . HashMap.elems $ stabilize (step2 (visible state bounds)) state

solve :: String -> IO ()
solve filepath = do
  Right (xs, bounds) <- parseOnly dataparser <$> BS.readFile filepath
  print $ solve1 xs
  print $ solve2 xs bounds
