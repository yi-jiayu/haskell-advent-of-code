import           Data.List
import           System.Environment
import           Welcome

move :: Char -> Int
move '(' = 1
move ')' = -1
move _ = error "err: invalid input"

climb :: (Int, Int) -> Char -> (Int, Int)
climb (step, floor) change
  | floor == -1 = (step, floor)
  | change == '(' = (step + 1, floor + 1)
  | change == ')' = (step + 1, floor -1)
  | otherwise = invalid

usage :: IO ()
usage = putStrLn "Usage: day_01.exe path/to/input"

soln :: [String] -> IO ()
soln args = do input <- readFile (head args)
               welcome 1
               putStrLn "Santa will end up at floor:"
               print $ sum (map move input)
               putStrLn "Santa enters the basement for the first time at position:"
               print $ fst $ foldl' climb (0, 0) input

main = do args <- getArgs
          if null args
            then usage
            else soln args
