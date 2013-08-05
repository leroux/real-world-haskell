module Ch4 where

import Data.Char (digitToInt)
import Data.List (foldl')
import System.Environment (getArgs)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail (_:xs) = Just xs
safeTail _      = Nothing

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit []     = Nothing
safeInit (x:xs) = Just $ x : init xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = seg : splitWith p rest
    where (seg, afterbreak) = break p xs
          rest = dropWhile p afterbreak

firstWord :: String -> String
firstWord = head . words

interactWith :: (String -> String) -> String -> IO ()
interactWith f s = putStrLn $ f s

putFirstWord :: IO ()
putFirstWord = do
    args <- getArgs
    case args of
        [line] -> interactWith firstWord line
        _ -> error "requires a single argument."

myTranspose :: [[a]] -> [[a]]
myTranspose [] = []
myTranspose ([] : xss) = myTranspose xss
myTranspose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : myTranspose (xs : [t | (_:t) <- xss])

asInt_fold :: String -> Int
asInt_fold [] = error "asInt_fold: empty string"
asInt_fold "-" = error "asInt_fold: invalid string"
asInt_fold s@(x:xs)
    | x == '-' = negate $ asInt_fold xs
    | otherwise = foldl' (\acc y -> (acc * 10) + digitToInt y) 0 s

myConcat :: [[a]] -> [a]
myConcat = foldr1 (++)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
    | p x = x : myTakeWhile p xs
    | otherwise = []

myTakeWhile' :: (a -> Bool) -> [a] -> [a]
myTakeWhile' p = foldr (\x acc -> if p x then x : acc else acc) []

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy _ [] = []
myGroupBy p (h:t) = foldr (\x (curr@(y:_):acc) -> if p x y then (x:curr) : acc else [x] : curr : acc) [[h]] t

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\x acc -> p x || acc) False

myCycle :: [a] -> [a]
myCycle xs = foldr (\x _ -> x ++ myCycle x) [] [xs]

myWords :: String -> [String]
myWords = filter (/= "") . foldr (\x (curr:acc) -> if x /= ' ' then (x:curr):acc else []:curr:acc) [[]]

myUnlines :: [String] -> String
myUnlines = foldr (\x acc -> x ++ "\n" ++ acc) []
