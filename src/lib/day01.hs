-- | 

module Day01 where

-- the provided example from the instructions
exampleExpenseReport :: [Integer]
exampleExpenseReport = [1721, 979, 366, 299, 675, 1456]

-- a tricky expense report to ensure that entries can't be summed up with themselves
anotherExpenseReport :: [Integer]
anotherExpenseReport = [1010, 2000, 20]

twoSum2020 :: [Integer] -> Integer
twoSum2020 (expenseReport) =
  let indexedExpenses = zip [0 :: Integer ..] expenseReport in
  head [x * y | (i, x) <- indexedExpenses, (j, y) <- indexedExpenses, x + y == 2020, i /= j]

input :: IO [Integer]
input = do
  raw <- readFile "data/Day01.txt"
  let someIntegers = map read . lines $ raw
  return someIntegers

part1 :: IO ()
part1 = do
  inp <- input
  print (twoSum2020 inp)

threeSum2020 :: [Integer] -> Integer
threeSum2020 (expenseReport) =
  let indexedExpenses = zip [0 :: Integer ..] expenseReport in
  head [x * y * z |
        (i, x) <- indexedExpenses,
        (j, y) <- indexedExpenses,
        (k, z) <- indexedExpenses,
        x + y + z == 2020,
        i /= j,
        j /= k,
        j /= k]

part2 :: IO ()
part2 = do
  inp <- input
  print (threeSum2020 inp)
