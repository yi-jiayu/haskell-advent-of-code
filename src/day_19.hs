import System.Environment
import Data.Char

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

main = do (inpFile:_) <- getArgs
          input <- readFile inpFile
          let medicineMolecue = (head . lines) input
          putStrLn "Number of steps to go from e to medicine molecule:"
          print $ (steps . molecues) medicineMolecue
