main :: IO ()
main = interact $ interactWith charCount

lineCount :: String -> Int
lineCount = length . lines

wordCount :: String -> Int
wordCount = length . words

charCount :: String -> Int
charCount = length

interactWith :: Show b => (a -> b) -> a -> String
interactWith with = (++ "\n") . show . with
