-- | 

module Day09 where

import Data.List.Split
import Debug.Trace as D (trace)

exampleInput :: [Int]
exampleInput = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]

valid :: [Int] -> Int -> Bool
valid prevs n =
  length [True | x <- prevs, y <- prevs, x + y == n, x /= y] > 0

validWindow :: Int -> [Int] -> Bool
validWindow _ [] = True
validWindow size xs@(_:rest)
  | length(xs) < size + 1 = True
  | otherwise =
        let ys = take size xs
            y  = xs !! size
        in valid ys y && validWindow size rest
        
window' :: Int -> [Int] -> [[Int]] -> [[Int]]
window' size xs acc
  | length xs < size = acc
  | otherwise = acc ++ [(take size xs)] ++ (window' size (tail xs) acc)

findInvalid :: Int -> [Int] -> Maybe Int
findInvalid size xs =
  let valid' = validWindow size
  in foldl (\acc e -> case acc of
               Nothing -> if valid' e then Nothing else Just (e !! size)
               _ -> acc
           ) Nothing (window' (size + 1) xs [])
     
part1 :: IO ()
part1 = do
  raw <- readFile "data/day09.txt"
  let numbers = map (\str -> read str :: Int) $ splitOn "\n" raw
  print $ findInvalid 25 numbers

--- part2

contiguousSum :: Int -> [Int] -> Int
contiguousSum n xs =
  let f _ _ _ [] = []
      f acc n' (y:ys) zs | sum acc + y == n' = acc ++ [y]
                         | sum acc + y < n' = f (acc ++ [y]) n' ys zs
                         | sum acc + y > n' = f [] n' (tail zs) (tail zs)
      numbers = f [] n xs xs
  in minimum numbers + maximum numbers

part2 :: IO ()
part2 = do
  raw <- readFile "data/day09.txt"
  let numbers = map (\str -> read str :: Int) $ splitOn "\n" raw
  print $ contiguousSum 1504371145 numbers
