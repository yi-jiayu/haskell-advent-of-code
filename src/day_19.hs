import           Data.Char
import           System.Environment
import           Welcome

splitMolecues :: ([String], Char) -> Char -> ([String], Char)
splitMolecues accum next
  | isUpper prev && isUpper next = (molecues ++ [[next]], next)
  | isUpper prev && isLower next = (init molecues ++ [last molecues ++ [next]], next)
  | isLower prev = (molecues ++ [[next]], next)
  where molecues = fst accum
        prev = snd accum

molecues :: String -> [String]
molecues = fst . foldl splitMolecues ([], 'A')

count :: (Eq a) => [a] -> a -> Int
count list e = (length . filter (== e)) list

-- based on explanation from https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju
steps :: [String] -> Int
steps mol = length mol  - count mol "Rn" - count mol "Ar" - 2 * count mol "Y" - 1

usage :: IO ()
usage = putStrLn "Usage: day_19.exe path/to/input"

soln :: [String] -> IO ()
soln args = do input <- readFile (head args)
               let medicineMolecue = (head . lines) input
               welcome 19
               putStrLn "Number of steps to go from e to medicine molecule:"
               print $ (steps . molecues) medicineMolecue


main = do args <- getArgs
          if null args then usage
          else soln args
