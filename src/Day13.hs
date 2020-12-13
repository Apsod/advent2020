module Day13(solve) where

import Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

type Input = (Integer, [Maybe Integer])

dataparser :: Parser Input
dataparser = (,) <$> (decimal <* endOfLine) <*> ((busparser `sepBy` ",") <* skipSpace <* endOfInput)
    where busparser = (fmap Just decimal) <|> ("x" *> pure Nothing)


solve1 :: Input -> Integer
solve1 (start, intervals) = uncurry (*) . minimum $ (\x -> (mod (-start) x, x)) <$> catMaybes intervals

solve2 :: Input -> Cycler
solve2 = foldMap (uncurry Cycler) . catMaybes . zipWith (\c mx -> (,c) <$> mx) [0..] . snd

getValue :: Cycler -> Integer
getValue (Cycler cycle position) = cycle - position

data Cycler = Cycler Integer Integer
    deriving Show

instance Semigroup Cycler where
    (Cycler c0 d0) <> (Cycler c1 d1) = Cycler c' d' 
        where
            c' = lcm c0 c1
            d' = (((d1 - d0) * inverse c1 c0) * c0 + d0) `mod` c'

instance Monoid Cycler where
    mempty = Cycler 1 0 


inverse q 1 = 1
inverse q p = (n * q + 1) `div` p where n = p - inverse p (q `mod` p)


solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print . solve1 $ xs
  print . getValue . solve2 $ xs
