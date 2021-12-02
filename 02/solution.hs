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
calculatePosition ((movement, value):xs) (hPos, depth)
  | movement == "forward" = calculatePosition xs (hPos + value, depth)
  | movement == "down" = calculatePosition xs (hPos, depth + value)
  | movement == "up" = calculatePosition xs (hPos, depth - value)

betterDescribePosition :: [(String, Int)] -> (Int, Int)
betterDescribePosition positions =
  betterCalculatePosition positions (0, 0, 0)

betterCalculatePosition :: [(String, Int)] -> (Int, Int, Int) -> (Int, Int)
betterCalculatePosition [] (hPos, depth, _) = (hPos, depth)
betterCalculatePosition ((movement, value):xs) (hPos, depth, aim)
  | movement == "forward" =
      betterCalculatePosition xs (hPos + value, depth + (aim * value), aim)
  | movement == "down" =
      betterCalculatePosition xs (hPos, depth, aim + value)
  | movement == "up" =
      betterCalculatePosition xs (hPos, depth, aim - value)
