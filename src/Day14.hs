module Day14 where

import Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.Bits
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data Instruction 
    = Mask Int Int
    | Write Int Int
        deriving Show 


type State = (Int, Int, IntMap Int)


runInstruction1 :: Instruction -> State -> State
runInstruction1 (Mask mask onoff) (_, _, memory) = (mask, onoff, memory)
runInstruction1 (Write ptr value) (mask, onoff, memory) =
    let value' = (mask .&. onoff) .|. (value .&. complement onoff)
    in (mask, onoff, IntMap.insert ptr value' memory)


runInstruction2 :: Instruction -> State -> State
runInstruction2 (Mask mask onoff) (_, _, memory) = (mask, onoff, memory)
runInstruction2 (Write ptr value) (mask, onoff, memory) =
    let ptr0 = (mask .|. ptr) .&. onoff
        variants = flipOnes (complement onoff)
        writes = IntMap.fromList . fmap (\p' -> (ptr0 .|. p', value)) $ variants
    in (mask, onoff, IntMap.union writes memory)


flipOnes :: Int -> [Int]
flipOnes x = go 35 [0]
    where
        go ix acc = case (ix >= 0, testBit x ix) of
            (True, True)  -> go (ix-1) (acc >>= (\x' -> [x', x' + bit ix]))
            (True, False) -> go (ix-1) acc
            (False, _) -> acc


dataparser :: Parser [Instruction]
dataparser = (lineparser `sepBy` endOfLine) <* skipSpace <* endOfInput
    where
        lineparser = maskparser <|> writeparser
        maskparser = uncurry Mask . BS.foldl maskfold (0, 0) <$> ("mask = " *> P.take 36)
        maskfold (mask, onoff) c = case c of
            '1' -> (mask' + 1, onoff' + 1)
            '0' -> (mask', onoff' + 1)
            'X' -> (mask', onoff')
            where 
                mask' = mask `shift` 1
                onoff' = onoff `shift` 1
        writeparser = Write <$> ("mem[" *> decimal <* "] = ") <*> decimal

getSum :: State -> Int
getSum (_, _, memory) = sum $ IntMap.elems memory

solve1 :: [Instruction] -> State
solve1 = foldl (flip runInstruction1) (0, 0, IntMap.empty)

solve2 :: [Instruction] -> State
solve2 = foldl (flip runInstruction2) (0, 0, IntMap.empty)

solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print . getSum . solve1 $ xs
  print . getSum . solve2 $ xs
