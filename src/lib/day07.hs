-- | 

module Day07 where

import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map

exampleInput :: String
exampleInput =
  "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
\dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
\bright white bags contain 1 shiny gold bag.\n\
\muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
\shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
\dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
\vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
\faded blue bags contain no other bags.\n\
\dotted black bags contain no other bags."

data Bag = Bag {bagName :: String, bagContent :: [Bag]} deriving (Show)

parseLine :: String -> (String, [(Int, String)])
parseLine str =
  let parts = splitOn " contain " str
      name = case splitOn " " (head parts) of
        (x:y:_) -> x ++ " " ++ y
        _ -> error "can't parse bag identifier"
      containers = parseContainersPart $ head . tail $ parts
  in (name, containers)

parseContainersPart :: String -> [(Int, String)]
parseContainersPart str =
  let statements = splitOn ", " str
      parse statement =
        case splitOn " " statement of
          ("no":_) -> Nothing
          (count:adjective:color:_) -> Just ((read count :: Int), (adjective ++ " " ++ color))
          _ -> Nothing
  in catMaybes $ map parse statements

parseInput :: String -> Map.Map String [(Int, String)]
parseInput str =
  let pairs = map parseLine (lines str)
  in foldl (\m (name, spec) -> Map.insert name spec m) Map.empty pairs

canHoldDirectly :: String -> [(Int, String)] -> Bool
canHoldDirectly str [] = False
canHoldDirectly str ((_, name):xs) = str == name || (canHoldDirectly str xs)

expand :: (Int, String) -> Map.Map String [(Int, String)] -> [(Int, String)]
expand (count, name) all = case Map.lookup name all of
  Just pairs -> map (\(c, n) -> (count * c, n)) pairs
  Nothing -> []

merge :: (Int, String) -> (Int, String) -> (Int, String)
merge (c, name) (c', name')
  | name == name' = (c+c', name)
  | otherwise = error "can't merge names differs"

compact :: [(Int, String)] -> [(Int, String)]
compact [] = []
compact (x@(count, name):xs) =
  let same = filter (\(_, name') -> name == name') xs
      others = filter (\(_, name') -> name /= name') xs
      merged = foldl merge x same
  in [merged] ++ compact others

expandAll :: [(Int, String)] -> Map.Map String [(Int, String)] -> [(Int, String)]
expandAll xs all = foldl (\acc e -> acc ++ (expand e all)) [] xs

canHold :: String -> [(Int, String)] -> Map.Map String [(Int, String)] -> Bool
canHold str [] all = False
canHold str specs all =
  (canHoldDirectly str specs) || (canHold str (expandAll specs all) all)

countCanHold :: String -> Map.Map String [(Int, String)] -> Int
countCanHold str all =
  let xs = Map.toList all
  in length $ filter id $ map (\(_, specs) -> canHold str specs all) xs

part1 :: IO ()
part1 = do
  raw <- readFile "data/day07.txt"
  let allSpecs = parseInput raw
  print $ countCanHold "shiny gold" allSpecs

--- part2

countDirectBags :: [(Int, String)] -> Int
countDirectBags = foldl (\acc (count, _) -> acc + count) 0

isLeaf :: String -> Map.Map String [(Int, String)] -> Bool
isLeaf str all = case Map.lookup str all of
  Just [] -> True
  _ -> False

countRecurBags :: [(Int, String)] -> Map.Map String [(Int, String)] -> Int
countRecurBags [] _ = 0
countRecurBags (x@(c, name):xs) all
  | isLeaf name all = c + countRecurBags xs all
  -- let's not forget to count the bag we are in
  | otherwise = c + countRecurBags ((compact $ expand x all) ++ xs) all 

countBags :: String -> Map.Map String [(Int, String)] -> Int
countBags str all = case Map.lookup str all of
  Just pairs -> countRecurBags pairs all
  Nothing -> 0

part2 :: IO ()
part2 = do
  raw <- readFile "data/day07.txt"
  let allSpecs = parseInput raw
  print $ countBags "shiny gold" allSpecs
