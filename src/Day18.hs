module Day18 (solve) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Attoparsec.ByteString.Char8 as Parsec


--data Rose a
--    = Branch [Rose a]
--    | Leaf a
--    deriving Show

data Expression
    = Add Expression Expression
    | Mul Expression Expression
    | Lit Int

parseline1 :: Parser Int
parseline1 = go id (skipSpace *> endOfInput)
    where
        rpar :: Parser ()
        rpar = () <$ ")"
        go :: (Int -> Int) -> Parser () -> Parser Int
        go f expect = do
            lhs <- f <$> choice [
                decimal,
                "(" *> go id rpar]
            choice [
                lhs <$ expect,
                " + " *> go (lhs +) expect,
                " * " *> go (lhs *) expect]

parseline2 :: Parser Int
parseline2 = go id (skipSpace *> endOfInput)
    where
        rpar :: Parser ()
        rpar = () <$ ")"
        go :: (Int -> Int) -> Parser () -> Parser Int
        go f expect = do
            lhs <- choice [
                decimal,
                "(" *> go id rpar]
            choice [
                (f lhs) <$ expect,
                " + " *> go (f . (lhs +)) expect,
                " * " *> go (f lhs *) expect]


solve1 :: ByteString -> Either String Int
solve1 = fmap sum . mapM (parseOnly parseline1) . ByteString.lines

solve2 :: ByteString -> Either String Int
solve2 = fmap sum . mapM (parseOnly parseline2) . ByteString.lines


solve :: String -> IO ()
solve filepath = do
    txt <- ByteString.readFile filepath
    print $ solve1 txt
    print $ solve2 txt
