module Ch4 where

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
