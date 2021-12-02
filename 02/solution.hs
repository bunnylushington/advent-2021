module Advent02
  where

import System.IO

main = do
  contents <- readFile "input-01"
  let positions =
        map (\[a, b] -> (a, read b :: Int)) $ map words $ lines contents
      (hPos, depth) = describePosition positions
      (betterPos, betterDepth) = betterDescribePosition positions
  putStrLn ("Answer One: " ++ show (hPos * depth))
  putStrLn ("Answer Two: " ++ show (betterPos * betterDepth))

describePosition :: [(String, Int)] -> (Int, Int)
describePosition positions =
  calculatePosition positions (0, 0)

calculatePosition :: [(String, Int)] -> (Int, Int) -> (Int, Int)
calculatePosition [] position = position
calculatePosition ((movement, value):xs) (hPos, depth) =
  let newPosition = case movement of
                      "forward" -> (hPos + value, depth);
                      "down"    -> (hPos, depth + value);
                      "up"      -> (hPos, depth - value)  in
    calculatePosition xs newPosition

betterDescribePosition :: [(String, Int)] -> (Int, Int)
betterDescribePosition positions =
  betterCalculatePosition positions (0, 0, 0)

betterCalculatePosition :: [(String, Int)] -> (Int, Int, Int) -> (Int, Int)
betterCalculatePosition [] (hPos, depth, _) = (hPos, depth)
betterCalculatePosition ((movement, value):xs) (hPos, depth, aim) =
  let newPosition = case movement of
                      "forward" -> (hPos + value, depth + (aim * value), aim);
                      "down"    -> (hPos, depth, aim + value);
                      "up"      -> (hPos, depth, aim - value) in
    betterCalculatePosition xs newPosition
