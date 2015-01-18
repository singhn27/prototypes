
module Main where

{-
  Contig Construction with 
  Needleman/Wunsch Sequence Alignment
-}

{- Modules -}

import Control.Applicative
import Data.Array
import Data.Function
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Ord
import System.Environment

{- Functions -}

-- Contig Construction --

{-
  Adapted from Vicuna viral genomics software
  http://www.broadinstitute.org/scientific-community/science/projects/viral-genomics/vicuna
-}

-- Kmer Subdivision --

mer :: Int -> [a] -> [[a]]
mer size ls = 
    case ls of 
        [] -> []
        x:xs -> 
            if length ls >= size then 
                (take size ls) : mer size xs
            else mer size xs

-- Hashing --

hashes :: Hashable a => Int -> [a] -> [([a], Int)]
hashes size ls = sortBy (comparing snd) $ zip (mer size ls) (map hash $ mer size ls)

newString :: [([a], Int)] -> [a]
newString z = concat $ concat $ map (\(f,s) -> f:[]) z

minHash :: [([a], Int)] -> Int
minHash ls = snd $ head ls

-- Sequence Tokenization --

tokens :: [Char] -> [([Char], Int, Int)]
tokens xs = map (\(s,l) -> (s, head l, last l)) $ zip sequences indices where
    sequences   = contSubSeqs xs
    indices     = contSubSeqs [0..length(xs)-1]
    contSubSeqs = filter (not . null) . concatMap inits . tails

-- Needleman/Wunsch Sequence Alignment --

{-
  Adapted from http://chneukirchen.org
-}

align :: String -> String -> [String]
align da db = format $ reverse $ traceback lena lenb where
    lena = length da
    lenb = length db
    a = ' ' : da
    b = ' ' : db
    memscore = listArray ((0, 0),(lena, lenb))
                         [score x y | x <- [0..lena], y <- [0..lenb]]
    infix 5 @@
    (@@) i j = memscore ! (i, j)
    score 0 _ = 0
    score _ 0 = 0
    score x y = maximum [(x - 1 @@ y - 1) + difference x y,
                          x - 1 @@ y,
                          x     @@ y - 1]
        where difference x y | a !! x == b !! y  = 1
                             | otherwise         = 0
    traceback 0 0 = []
    traceback x y | x       == 0           = (' ', b !! y)    : traceback  0       (y - 1)
                  | y       == 0           = (a !! x, ' ')    : traceback  (x - 1) 0
                  | x @@ y  == x @@ y - 1  = (' ', b !! y)    : traceback  x       (y - 1)
                  | x @@ y  == x - 1 @@ y  = (a !! x, ' ')    : traceback  (x - 1) y
                  | otherwise              = (a !! x, b !! y) : traceback  (x - 1) (y - 1)
    format l = [map fst l, map snd l]

-- Perform Analysis --

main :: IO ()
main = do
    args <- getArgs
    file <- readFile $ head args
    let size        = read $ args !! 1
    let allLines    = lines file
    let seqs        = map snd . filter (\(x,y) -> (mod x 2) == 0) . zip [1..] $ allLines
    let base        = map (\x -> hashes size $ newString $ hashes size x) seqs
    let dict        = map (\(a,b) -> (b,a)) $ concat base
    let minHashes   = map minHash base
    let cmpHashes   = sortBy (comparing fst) $ zip minHashes [1..]
    let gpdHashes   = groupBy ((==) `on` fst) cmpHashes
    let lngHashes   = filter (\x -> length x > 1) gpdHashes
    let relSeqs     = map (concat . map (\(a,b) -> b : [])) lngHashes
    let indices     = map nub $ map (\x -> map fst x) lngHashes
    let collected   = map (\(a,b) -> (dictLkup a, crossRefs b)) $ zip (concat indices) relSeqs where
        dictLkup a  = (fromJust $ lookup a dict)
        crossRefs b = (map (\x -> (lines file) !! (x - 1)) b)
    let paired      = zip <*> tail $ map fst collected
    let alignments  = nub $ concat $ map (\(a,b) -> align a b) paired
    let rankings    = map spaces $ alignments where
        spaces      = length . filter (== ' ')
    let sortedAlg   = sortBy (comparing fst) $ zip rankings alignments
    print sortedAlg
