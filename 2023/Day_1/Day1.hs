import Data.Char (isDigit)
import Data.List (elemIndex)
import System.Posix (fileExist)

describedDigits = [
  "zero",
  "one",
  "two",
  "three",
  "four", 
  "five",
  "six", 
  "seven", 
  "eight", 
  "nine"
  ]

maxDigitLength :: Int
maxDigitLength = foldl (\maxLength digit -> max maxLength (length digit)) 0 describedDigits


main :: IO ()
main = do
  content <- readFile "aoc_day_1_second_input.txt"
  let input = lines content
  let digits = map captureFirstAndLastDigit input
  let amount = sum digits
  _ <- print amount
  return ()

isDescribedDigit :: String -> Bool
isDescribedDigit digit = length digit > 2 && digit `elem` describedDigits

isRegularDigit :: String -> Bool
isRegularDigit digit = length digit == 1 && isDigit (head digit)

isDigit' :: String -> Bool
isDigit' digit = isRegularDigit digit || isDescribedDigit digit

findFirstDigit :: String -> Int -> String
findFirstDigit word offset
  | isDigit' digit = digit
  | limit == 0 = findFirstDigit (drop 1 word) 0
  | otherwise = findFirstDigit word (offset + 1)
  where 
    limit = maxDigitLength - offset
    digit = take limit word

findLastDigit :: String -> Int -> String
findLastDigit word offset
  | isDigit' digit = digit
  | limit == 0 = findLastDigit (take (length word - 1) word) 0
  | otherwise = findLastDigit word (offset + 1)
  where
    limit = maxDigitLength - offset
    digit = take limit (drop (length word - limit) word)

parseDescribedDigit :: String -> Maybe String
parseDescribedDigit digit = case elemIndex digit describedDigits of
  Just value -> Just (show value)
  Nothing -> fallback
    where
      fallback 
        | isRegularDigit digit = Just digit
        | otherwise = Nothing

captureFirstAndLastDigit :: String -> Int
captureFirstAndLastDigit word = 
  case (parseDescribedDigit firstDigit, parseDescribedDigit lastDigit) of 
    (Nothing, Just last) -> read last 
    (Just first, Nothing) -> read first
    (Just first, Just last) -> read (first <> last)
  where
    firstDigit = findFirstDigit word 0
    lastDigit = findLastDigit word 0
