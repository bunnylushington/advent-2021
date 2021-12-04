module Advent03
  where

import System.IO
import Data.List (elemIndices, foldl', intersect)

type Sample = ([String],[String])
type SampleList = [Sample]

type Comp = (Int -> Int -> Bool)

data Bit = Zero | One
  deriving (Show, Eq)


main = do
  contents <- readFile "input-01"
  partOne contents
  partTwo contents

-- Part One --------------------

partOne :: String -> IO ()
partOne contents = do
  let samples = lines contents
      initialCounts = makeCountList $ (length $ head samples) - 1
      finalCounts = foldl' parseLine initialCounts samples
      gamma = binaryToInteger $ determineGammaRate finalCounts
      episilon = binaryToInteger $ determineEpisilonRate finalCounts
  putStrLn ("Part One: " ++ (show $ gamma * episilon))

makeCountList len =
  map (\i -> (i, 0, 0)) [0 .. len]

parseLine :: [(Int, Int, Int)] -> String -> [(Int, Int, Int)]
parseLine counts line =
  let zeros = elemIndices '0' line in
    map (\(i, a, b) -> case elem i zeros of
                         True -> (i, a+1, b);
                         False -> (i, a, b+1)) counts

determineRate :: (Int -> Int -> Bool) -> [(Int, Int, Int)] -> String
determineRate fn counts =
  concat $ map (\(_, a, b) -> case fn a b of
                                True -> "0";
                                False -> "1") counts

determineGammaRate :: [(Int, Int, Int)] -> String
determineGammaRate = determineRate (>)

determineEpisilonRate :: [(Int, Int, Int)] -> String
determineEpisilonRate = determineRate (<)


-- Part Two --------------------

partTwo :: String -> IO ()
partTwo contents = do
  let samples = lines contents
      oxygen = binaryToInteger $ head $ oxygenRating samples
      scrubber = binaryToInteger $ head $ scrubberRating samples
  putStrLn $ "Part Two: " ++ show (oxygen * scrubber)

oxygenRating :: [String] -> [String]
oxygenRating samples =
  foldl' (bitFilter One (>)) samples [0 .. (length $ head samples) - 1]

scrubberRating :: [String] -> [String]
scrubberRating samples =
  foldl' (bitFilter Zero (<)) samples [0 .. (length $ head samples) - 1]

bitFilter :: Bit -> Comp -> [String] -> Int -> [String]
bitFilter keepBit comp samples position
  | (length samples) == 1 = samples
  | otherwise =
      chooseSamples comp keepBit $ foldl' (splitSamples position) ([],[]) samples

splitSamples :: Int -> Sample -> String -> Sample
splitSamples position (zeros, ones) sample =
  case (sample !! position) of
    '0' -> ([sample] ++ zeros, ones);
    '1' -> (zeros, [sample] ++ ones)

chooseSamples :: Comp -> Bit -> Sample -> [String]
chooseSamples comp keepBit (zeros, ones)
  | (length zeros) == (length ones) = case keepBit of
                                        Zero -> zeros;
                                        One -> ones
  | comp (length zeros) (length ones) = zeros
  | otherwise = ones


-- Common Utilities


binaryToInteger :: String -> Int
binaryToInteger bin =
  let (_, total) = foldr binInt (0, 0) bin in total

binInt :: Char -> (Int, Int) -> (Int, Int)
binInt char (pos, total) =
  case char of
    '0' -> (pos+1, total);
    '1' -> (pos+1, total + (2^pos))
