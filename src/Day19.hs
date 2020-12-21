module Day19 (solve) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Attoparsec.ByteString.Char8 as Parsec
import Control.Applicative
import Data.List (sortBy, splitAt)
import Data.Ord (comparing)
import Data.Maybe
import Control.Monad

newtype Fix f = Fix {unFix :: f (Fix f)}

data Rule
    = Seq [[Int]]
    | Match Char


parseRule :: Parser (Int, Rule)
parseRule = (,) <$> (decimal <* ": ") <*> (charparser <|> seqparser)
    where
        charparser :: Parser Rule
        charparser = do
            "\""
            c <- anyChar
            "\""
            return $ Match c

        seqparser :: Parser Rule
        seqparser = do
            lists <- (decimal `sepBy` " ") `sepBy` " | "
            return $ Seq lists

fix :: [Rule] -> Rule -> ByteString -> [ByteString]
fix rules = go 
    where
        go (Match char) txt = do 
            (h, t) <- maybeToList $ ByteString.uncons txt
            [t | h == char]
        go (Seq seqs) txt = seqs >>= foldM (\t ix -> go (rules!!ix) t) txt

parseRules :: Parser [Rule]
parseRules = fmap snd . sortBy (comparing fst) <$> (parseRule `sepBy` endOfLine)


put :: Int -> a -> [a] -> [a]
put ix x xs = 
    let (l, _:r) = splitAt ix xs
    in l ++ (x:r) 

r8 :: Rule 
r8 = Seq [[42], [42, 8]]

r11 :: Rule
r11 = Seq [[42, 31], [42, 11, 31]]

valid :: [Rule] -> ByteString -> Bool
valid rules = elem "" . fix rules (head rules)


solve12 txt = 
    let (rule_txt, rest) = ByteString.breakSubstring "\n\n" txt
        txts = ByteString.lines . ByteString.drop 2 $ rest
        Right rules = parseOnly parseRules rule_txt
        rules2 = put 8 r8 . put 11 r11 $ rules
    in do 
        print . length $ filter (valid rules) txts 
        print . length $ filter (valid rules2) txts 

solve :: String -> IO ()
solve filepath = do
    txt <- ByteString.readFile filepath
    solve12 txt
