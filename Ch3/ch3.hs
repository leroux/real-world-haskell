import Data.List

main :: IO ()
main = undefined

type BookID = Int
type BookName = String
type BookContents = [String]

data BookInfo = Book Int BookName BookContents
              deriving Show

myInfo :: BookInfo
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody
                deriving Show
type BookRecord = (BookInfo, BookReview)

data Bool' = False | True

data List a = Nil
            | Cons a (List a)
            deriving Show

toList :: List a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs

data Tree a = Empty
            | Tree a (Tree a) (Tree a)
            deriving Show

---

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mean xs = s / l
  where s = sum xs
        l = fromIntegral $ length xs

mkPalindrome :: [a] -> [a]
mkPalindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (\a b -> length a `compare` length b)

myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ []     = []
myIntersperse _ [x]    = x
myIntersperse s (x:xs) = x ++ [s] ++ myIntersperse s xs

height :: Tree a -> Int
height Empty        = 0
height (Tree _ l r) = 1 + max (height l) (height r)

data Direction = LeftTurn | RightTurn | Straight
               deriving Show
