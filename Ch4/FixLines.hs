import System.Environment (getArgs)

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ f input

main :: IO ()
main = mainWith function
  where mainWith f = do
            args <- getArgs
            case args of
              [input, output] -> interactWith f input output
              _ -> error "Atleast two arguments are required."
        function = concat . replicate 4

splitLines :: String -> [String]
splitLines [] = []
splitLines xs =
    let (pre, suff) = break isLineTerminator xs
    in pre : case suff of
               ('\r':'\n':rest) -> splitLines rest
               ('\r':rest)      -> splitLines rest
               ('\n':rest)      -> splitLines rest
               _                -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'

fixLines :: String -> String
fixLines = unlines . splitLines
