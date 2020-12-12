module Day12 (solve) where

import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS
import Linear (V2(..), (^+^), (*^), (!*))
import Data.Monoid

data State = State (V2 Int) (V2 Int)
    deriving(Show)

manhattan :: State -> Int
manhattan (State p _) = getSum $ foldMap (Sum . abs) p

data Instruction
    = Forward Int
    | Rotate Int
    | Move (V2 Int)
    deriving(Show)

intCos :: Int -> Int
intCos x = go (x `mod` 360)
    where 
        go 0 = 1
        go 90 = 0
        go 180 = -1
        go 270 = 0

intSin :: Int -> Int
intSin x = go (x `mod` 360)
    where 
        go 0 = 0
        go 90 = 1
        go 180 = 0
        go 270 = -1


addv :: V2 Int -> V2 Int -> Int -> V2 Int
addv v delta alpha = v ^+^ (alpha *^ delta)

step1 :: Instruction -> State -> State
step1 (Forward units) (State position direction) = State (addv position direction units) direction
step1 (Rotate degrees) (State position direction) = State position (rotation !* direction)
    where 
        sin = intSin degrees
        cos = intCos degrees
        rotation = V2 (V2 cos (-sin)) (V2 sin cos)
step1 (Move delta) (State position direction) = State (position ^+^ delta) direction

step2 :: Instruction -> State -> State
step2 (Move delta) (State position direction) = State position (direction ^+^ delta)
step2 instr state = step1 instr state

state1 :: State
state1 = State (V2 0 0) (V2 1 0)

state2 :: State
state2 = State (V2 0 0) (V2 10 1)

dataparser ::  Parser [Instruction]
dataparser = (parseline `sepBy` endOfLine) <* skipSpace <* endOfInput
    where 
        moveparser = choice [
            Move . V2 0 <$> ("N" *> decimal),
            Move . flip V2 0 <$> ("E" *> decimal),
            Move . V2 0 . negate <$> ("S" *> decimal),
            Move . flip V2 0 . negate <$> ("W" *> decimal)
            ]
        rotparser = choice [
            Rotate <$> ("L" *> decimal),
            Rotate . negate <$> ("R" *> decimal)
            ]
        forwardparser = Forward <$> ("F" *> decimal)
        parseline = choice [moveparser, rotparser, forwardparser]

solve1 :: [Instruction] -> Int
solve1 = manhattan . foldl (flip step1) state1

solve2 :: [Instruction] -> Int
solve2 = manhattan . foldl (flip step2) state2
 
solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print $ solve1 xs
  print $ solve2 xs
