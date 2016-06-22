import           Data.List          (isInfixOf)
import           System.Environment

hasThreeVowels :: String -> Bool
hasThreeVowels x = 2 < length (filter isVowel x)

isVowel :: Char -> Bool
isVowel c = c `elem` ['a', 'e', 'i', 'o', 'u']

hasRepeatedLetter :: String -> Bool
hasRepeatedLetter s = fst $ foldl isRepeatedLetter (False, ' ') s

isRepeatedLetter :: (Bool, Char) -> Char -> (Bool, Char)
isRepeatedLetter accum next = let comp = fst accum || (snd accum == next)
                              in (comp, next)

noIllegalSubstrings :: String -> Bool
noIllegalSubstrings s
  | "ab" `isInfixOf` s = False
  | "cd" `isInfixOf` s = False
  | "pq" `isInfixOf` s = False
  | "xy" `isInfixOf` s = False
  | otherwise = True

isNiceString :: String -> Bool
isNiceString s = hasThreeVowels s
              && hasRepeatedLetter s
              && noIllegalSubstrings s

main :: IO ()
main = do (file:_) <- getArgs
          input <- readFile file
          putStrLn "Number of nice strings under old rules:"
          print $ length (filter isNiceString $ lines input)
          putStrLn "Number of nice strings under new rules:"
