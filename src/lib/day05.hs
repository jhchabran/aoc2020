-- | 

module Day05 where

import Data.List

exampleInput :: [(String, (Int, Int, Int))]
exampleInput = [
  ("BFFFBBFRRR", (70, 7, 567)),
  ("FFFBBBFRRR", (14, 7, 119)),
  ("BBFFBBFRLL", (102, 4,820))
  ]

seatID :: Int -> Int -> Int
seatID row col = row * 8 + col

plane :: (Int, Int)
plane = (128, 8) -- rows, cols

getRowSpec :: String -> String
getRowSpec raw = take 7 raw

getColSpec :: String -> String
getColSpec raw = drop 7 raw

getRow :: String -> (Int, Int) -> Int
getRow [] (a, _) = a
getRow ('F':xs) (a, b) = getRow xs (
  a,
  a + ((b - a) `div` 2) 
  )
getRow ('B':xs) (a, b) = getRow xs (
  a + ((b - a) `div` 2) + 1,
  b
  )
getRow _ _ = error "incorrect row spec"

getCol :: String -> (Int, Int) -> Int
getCol [] (a, _) = a
getCol ('L':xs) (a, b) = getCol xs (
  a,
  a + ((b - a) `div` 2) 
  )
getCol ('R':xs) (a, b) = getCol xs (
  a + ((b - a) `div` 2) + 1,
  b
  )
getCol _ _ = error "incorrect col spec"

parseSeat :: String -> Int
parseSeat spec =
  let row = getRow (getRowSpec spec) (0,127)
      col = getCol (getColSpec spec) (0,7)
  in seatID row col

part1 :: IO ()
part1 = do
  raw <- readFile "data/Day05.txt"
  let seats = map parseSeat $ lines raw
  print $ maximum seats

---

findMissingSeat :: [String] -> Int
findMissingSeat specs =
  let seats = sort $ map parseSeat specs
      f (x:x':xs) = if x' - x > 1 then x + 1 else f(x':xs)
  in f seats

part2 :: IO ()
part2 = do
  raw <- readFile "data/Day05.txt"
  print $ findMissingSeat $ lines raw
