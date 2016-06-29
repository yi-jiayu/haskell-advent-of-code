module Welcome (welcome, invalid, readInts) where

welcome :: Int -> IO ()
welcome day = do putStrLn "Haskell Advent of Code"
                 putStrLn "https://github.com/yi-jiayu/haskell-advent-of-code"
                 putStrLn ("Day " ++ show day ++ "\n")

invalid :: t
invalid = error "err: invalid input"

readInts :: [String] -> [Int]
readInts = map read
