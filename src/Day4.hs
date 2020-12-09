module Day4 (solve) where

import Data.Monoid
import Data.Functor
import Control.Applicative
import Data.Bool
import Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

oneSpace :: P.Parser ()
oneSpace = (void " ") <|> endOfLine

keys :: [BS.ByteString]
keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

between :: Ord a => a -> a -> a -> Bool
between lb ub x = lb <= x && x <= ub

invalid :: P.Parser Bool
invalid = const False <$> takeTill isSpace

lookit = lookAhead oneSpace

byr = (,) <$> ("byr" <* ":") <*> (check <|> invalid)
  where check = (between 1920 2002 . read <$> count 4 digit) <* lookit

iyr = (,) <$> ("iyr" <* ":") <*> (check <|> invalid)
  where check = (between 2010 2020 . read <$> count 4 digit) <* lookit

eyr = (,) <$> ("eyr" <* ":") <*> (check <|> invalid)
  where check = (between 2020 2030 . read <$> count 4 digit) <* lookit

hgt = (,) <$> ("hgt" <* ":") <*> (check <|> invalid)
  where
    check = (check' <$> decimal <*> ("cm" <|> "in")) <* lookit
    check' val "cm" = between 150 193 val
    check' val "in" = between 59 76 val

hcl = (,) <$> ("hcl" <* ":") <*> (check <|> invalid)
  where
    check = (const True <$> "#" <* (count 6 . satisfy $ inClass "0-9a-f")) <* lookit

ecl = (,) <$> ("ecl" <* ":") <*> (check <|> invalid)
  where check = (const True <$> choice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) <* lookit

pid = (,) <$> ("pid" <* ":") <*> (check <|> invalid)
  where check = (const True <$> count 9 digit) <* lookit

cid = (,) <$> ("cid" <* ":") <*> check
  where check = const True <$> invalid

required :: [BS.ByteString]
required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

type Info = M.Map BS.ByteString Bool

dataparser :: P.Parser [Info]
dataparser = (parsepassport `sepBy` (endOfLine *> endOfLine)) <* skipSpace <* endOfInput
  where
    parsepair :: P.Parser (BS.ByteString, Bool)
    parsepair = choice [byr, iyr, eyr, hgt, hcl, ecl, pid, cid]
    parsepassport = M.fromList <$> (parsepair `sepBy` oneSpace)

valid1 :: Info -> Bool
valid1 info = and $ (\k -> M.member k info) <$> required

valid2 :: Info -> Bool
valid2 info = valid1 info && (and $ M.elems info)

countmap :: Info -> (Sum Int, Sum Int)
countmap info =
  let 
    v1 = valid1 info
    v2 = v1 && (and $ M.elems info)
    bool2sum = Sum . bool 0 1
  in (bool2sum v1, bool2sum v2)


solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print $ foldMap countmap xs
