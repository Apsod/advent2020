{-# LANGUAGE ApplicativeDo #-}

module Day7 (solve) where

--import qualified Data.Set as S
--import qualified Data.Map as M
import Data.Bifunctor
import Data.Containers.ListUtils (nubOrd)
import Data.Graph as G
import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Maybe
import Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS
import Data.Array as A

type Adjective = BS.ByteString
type Color = BS.ByteString
type Bag = BS.ByteString

type GraphInfo = (Graph, M.Map (Int, Int) Int, M.Map Bag Int, M.Map Int Bag)

dataparser :: Parser GraphInfo
dataparser = to_graph <$> (lineparser `sepBy` endOfLine) <* skipSpace <* endOfInput
  where
    wordparser = P.takeWhile1 (inClass "a-z")

    bagparser :: Parser Bag
    bagparser = (\adj clr -> BS.concat [adj, " ", clr]) <$>
      (wordparser <* " ") <*> 
      (wordparser <* " " <* choice ["bags", "bag"])

    bagcontent :: Parser (Int, Bag)
    bagcontent = (\n bag -> (n, bag)) <$>
      (decimal <* " ") <*>
      bagparser

    contentparser = choice [nothing, something]
      where
        nothing = (const Nothing <$> "no other bags")
        something = Just <$> bagcontent `sepBy` ", "

    lineparser = (\outer contents -> (outer, fromMaybe [] contents)) <$>
      (bagparser <* " contain ") <*>
      (contentparser <* ".")

to_graph :: [(Bag, [(Int, Bag)])] -> (Graph, M.Map (Int, Int) Int, M.Map Bag Int, M.Map Int Bag)
to_graph bags =
  let
    bagset = S.fromList $ do
      (outer, inners) <- bags
      (n, inner) <- inners
      [outer, inner]

    (bag2int, int2bag) = index_elems bagset

    edges = do
      (outer, inners) <- bags
      (n, inner) <- inners
      let Just (o, i) = (,) <$> M.lookup outer bag2int <*> M.lookup inner bag2int
      return ((o, i), n)

    edge2weight = M.fromList edges

    bounds = (1, S.size bagset)
    graph = buildG bounds (fst <$> edges)
  in (graph, edge2weight, bag2int, int2bag)

index_elems :: Ord a => S.Set a -> (M.Map a Int, M.Map Int a)
index_elems xs = (a2i, i2a)
  where
    xs' = S.toList xs
    a2i = M.fromList $ zip xs' [1..]
    i2a = M.fromList $ zip [1..] xs'


solve1 :: GraphInfo -> Int
solve1 (graph, edge, bag2int, int2bag) = (\x -> (x-1)) . length . reachable graph' . fromJust $ M.lookup "shiny gold" bag2int
  where graph' = transposeG graph

solve2 :: GraphInfo -> Int
solve2 (graph, edge, bag2int, int2bag) =
  let
    graph' = transposeG graph
    toposort = topSort graph'
    v2top = M.fromList $ zip toposort [0..]
    total = fmap f toposort
      where
        f ix = (1+) . sum $ zipWith (\dep num -> total !! dep * num) deps' nums
          where
            deps = (A.!) graph ix
            deps' = fmap (\dep -> fromJust $ M.lookup dep v2top) deps
            nums = fmap (\dep -> fromJust $ M.lookup (ix, dep) edge) deps
  in fromJust $ do
    tix <- M.lookup "shiny gold" bag2int >>= \i -> M.lookup i v2top
    return (total !! tix)


solve :: String -> IO ()
solve filepath = do
  Right xs <- parseOnly dataparser <$> BS.readFile filepath
  print $ solve1 xs
  print $ solve2 xs - 1

