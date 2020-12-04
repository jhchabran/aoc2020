-- | 

module Day01 where

import Data.List.Split
import Text.Regex.PCRE
import Text.Read

mandatoryFields :: [String]
mandatoryFields = [
  "byr",
  "iyr",
  "eyr",
  "hgt",
  "hcl",
  "ecl",
  "pid"
  -- "cid" -- ok but actually okay to not have it
  ]

exampleInput :: String
exampleInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
\byr:1937 iyr:2017 cid:147 hgt:183cm\n\
\\n\
\iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
\hcl:#cfa07d byr:1929\n\
\\n\
\hcl:#ae17e1 iyr:2013\n\
\eyr:2024\n\
\ecl:brn pid:760753108 byr:1931\n\
\hgt:179cm\n\
\\n\
\hcl:#cfa07d eyr:2025 pid:166559648\n\
\iyr:2011 ecl:brn hgt:59in"

parseKV :: String -> (String, String)
parseKV str =
  let vals = splitOn ":" str
  in (vals !! 0, vals !! 1)

parseLine :: String -> [(String, String)]
parseLine str = map parseKV $ splitOn " " str

parsePassport :: String -> [(String, String)]
parsePassport str =
  let rows = lines str
  in foldl (\pairs line -> pairs ++ (parseLine line)) [] rows

splitRawPassports = splitOn "\n\n"

parsePassports :: String -> [[(String, String)]]
parsePassports raw =
  map parsePassport $ splitRawPassports raw

isPassportValid :: [(String, String)] -> [String] -> Bool -> Bool
isPassportValid _ _ False = False
isPassportValid _ [] True = True
isPassportValid fields (x:xs) True =
  let names = map fst fields
  in isPassportValid fields xs (x `elem` names)

isPassportValid' :: [(String, String)] -> Bool
isPassportValid' passport = isPassportValid passport mandatoryFields True
  
part1 :: IO ()
part1 = do
  raw <- readFile "data/Day04.txt"
  let passports = parsePassports raw
  print $ length $ filter (\b -> b) $ map isPassportValid' passports

-- part2

hexColorRE = "#[0-9a-f]{6}"

validate :: (String, String) -> Bool
validate ("byr", v)
  | year >= 1920 && year <= 2002 = True
  | otherwise = False
  where year = read v :: Int
validate ("iyr", v)
  | year >= 2010 && year <= 2020 = True
  | otherwise = False
  where year = read v :: Int
validate ("eyr", v)
  | year >= 2020 && year <= 2030 = True
  | otherwise = False
  where year = read v :: Int
validate ("hgt", v)
  | unit == "cm" && (n >= 150 && n <= 193) = True
  | unit == "in" && (n >= 59 && n <= 76) = True
  | otherwise = False
  where unit = reverse $ take 2 $ reverse v
        n = read (reverse $ drop 2 $ reverse v) :: Int
validate ("hcl", v) = v =~ hexColorRE
validate ("ecl", "amb") = True
validate ("ecl", "blu") = True
validate ("ecl", "brn") = True
validate ("ecl", "gry") = True
validate ("ecl", "grn") = True
validate ("ecl", "hzl") = True
validate ("ecl", "oth") = True
validate ("ecl", _) = False
validate ("pid", v) 
  | length v == 9 = case (readMaybe v) :: Maybe Integer of
      Nothing -> False
      Just _ -> True
  | otherwise = False
validate ("cid", _) = True
validate _ = False

isPassportValid'' :: [(String, String)] -> Bool
isPassportValid'' passport =
  let hasFields = isPassportValid' passport 
      fieldsValidity = map (\field -> validate field) passport
      hasValidFields = foldl (\b v -> b && v) True fieldsValidity
  in hasFields && hasValidFields

part2 :: IO ()
part2 = do
  raw <- readFile "data/Day04.txt"
  let passports = parsePassports raw
  print $ length $ filter (\b -> b) $ map isPassportValid'' passports
