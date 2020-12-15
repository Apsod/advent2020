module Day15(solve) where


import Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List

dataparser :: Parser [Int]
dataparser = (decimal `sepBy` char ',') <* skipSpace <* endOfInput

type State = ((Int, Int), IntMap Int)

getPrev = snd . fst
getTurn = fst . fst

put :: Int -> Maybe Int -> ((Int, Int), Maybe Int)
put turn = \case
    Just x -> ((turn + 1, turn - x), Just turn)
    Nothing -> ((turn + 1, 0), Just turn)

step :: State -> State
step ((turn, prev), spoken) = IntMap.alterF (put turn) prev spoken

initState :: [Int] -> State
initState xs = ((length xs - 1, last xs), IntMap.fromList $ zip (init xs) [0..])

solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  let 
    s0 = initState xs 
    t0 = getTurn s0
    ix1 = 2020 - t0 - 1
    ix2 = 30000000 - t0 - 1
  print . getPrev . (!! ix1) . iterate' step $! s0
  print . getPrev . (!! ix2) . iterate' step $! s0
