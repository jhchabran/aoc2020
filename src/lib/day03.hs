-- | 

module Day03 where

exampleMap :: [String]
exampleMap = [
  "..##.......",
  "#...#...#..",
  ".#....#..#.",
  "..#.#...#.#",
  ".#...##..#.",
  "..#.##.....",
  ".#.#.#....#",
  ".#........#",
  "#.##...#...",
  "#...##....#",
  ".#..#...#.#"
  ]

isTree :: Char -> Bool
isTree '#' = True
isTree _ = False

countTrees :: [String] -> Int -> Int -> Int
countTrees [] count _ = count
countTrees (_:[]) count _ = count -- there can't be a tree if we're out of slopes for the "down"
countTrees rows@(_:row:_) count x =
  let newX = (x + 3) `mod` (length row)
      here = row !! newX 
      newCount = if isTree here then count + 1 else count
  in countTrees (tail rows) newCount newX

input :: IO [String]
input = do
  raw <- readFile "data/Day03.txt"
  return $ lines raw

part1 :: IO ()
part1 = do
  inp <- input
  print $ countTrees inp 0 0

-- part2
slopes :: [(Int, Int)]
slopes = [
  (1,1),
  (3,1),
  (5,1),
  (7,1),
  (1,2)
  ]

countTrees' :: [String] -> (Int, Int) -> Int -> Int -> Int
countTrees' [] _ count _ = count
countTrees' rows slope@(right, down) count x 
  | length rows >= down + 1 =
    let newX = (x + right) `mod` (length row)
        row = rows !! down
        here = row !! newX
        newCount = if isTree here then count + 1 else count
    in countTrees' (drop down rows) slope newCount newX
  | otherwise = count

trees :: [String] -> (Int, Int) -> Int
trees rows slope = countTrees' rows slope 0 0 

part2 :: IO ()
part2 = do
  inp <- input
  print $ product $ map (\slope -> trees inp slope) slopes
