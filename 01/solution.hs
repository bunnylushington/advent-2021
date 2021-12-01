module Advent01
where

import System.IO

main = do
  contents <- readFile "input-01"
  let measurements = map (\a -> read a :: Int) $ lines contents
      simpleDepthCount = depthRead measurements
      slidingDepthCount = slidingRead measurements
  putStrLn ("simple count " ++ show simpleDepthCount)
  putStrLn ("sliding count " ++ show slidingDepthCount)

-- 10 units deeper than the deepest reading.  Gives us an initial value.
depthBound :: [Int] -> Int
depthBound values = 10 + (maximum values)

depthRead :: [Int] -> Int
depthRead values =
  dR values 0 (depthBound values)

dR :: [Int] -> Int -> Int -> Int
dR [] count _ = count
dR (x:xs) count last
  | x > last = dR xs (count + 1) x
  | otherwise = dR xs count x

slidingRead :: [Int] -> Int
slidingRead values =
  sR values [] 0 (depthBound values)

sR :: [Int] -> [Int] -> Int -> Int -> Int
sR [] _ count _ = count
sR (x:y:xs) [] count lastSlidingDepth = sR xs [x, y] count lastSlidingDepth
sR (x:xs) [a, b] count lastSlidingDepth
  | slidingDepth > lastSlidingDepth = sR xs newWindow (count + 1) slidingDepth
  | otherwise = sR xs newWindow count slidingDepth
  where
    slidingDepth = x + a + b
    newWindow = [b, x]
