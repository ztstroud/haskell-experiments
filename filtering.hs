-- Tests each element of the given list against the given
-- filter, and includes it in the output if it passes
filterer :: (a -> Bool) -> [a] -> [a]
filterer filter [] = []
filterer filter (a:as)
    | filter a == True = a : filterer filter as
    | otherwise        = filterer filter as
    
-- Returns true when the given number is positive
positiveFilter :: (Num n, Ord n) => n -> Bool
positiveFilter n
    | n > 0     = True
    | otherwise = False
    
-- Returns the when the given number is negative
negativeFilter :: (Num n, Ord n) => n -> Bool
negativeFilter n
    | n < 0     = True
    | otherwise = False
    
-- Returns true when the given string is shorter than
-- the given length
stringLengthFilter :: Int -> String -> Bool
stringLengthFilter len string = length string < len

-- An alternative to filterer positiveFilterer that uses
-- list comprehension
positiveFilterer :: (Num n, Ord n) => [n] -> [n]
positiveFilterer n = [x | x <- n, x > 0]