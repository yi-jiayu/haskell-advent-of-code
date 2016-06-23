import           Data.List          (foldl', isInfixOf, elemIndices)
import           System.Environment

hasThreeVowels :: String -> Bool
hasThreeVowels x = 2 < length (filter isVowel x)

isVowel :: Char -> Bool
isVowel c = c `elem` ['a', 'e', 'i', 'o', 'u']

hasRepeatedLetter :: String -> Bool
hasRepeatedLetter s = fst $ foldl' isRepeatedLetter (False, ' ') s

isRepeatedLetter :: (Bool, Char) -> Char -> (Bool, Char)
isRepeatedLetter accum next = let rep = fst accum || (snd accum == next)
                              in (rep, next)

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

hasRepeatedPair :: String -> Bool
hasRepeatedPair s = True `elem` let pairs = zip s (tail s)
                                in map (`isRepeatedPair` pairs) pairs

isRepeatedPair :: (Char, Char) -> [(Char, Char)] -> Bool
isRepeatedPair pair pairs
  | uncurry (==) pair = let ids = elemIndices pair pairs
                        in if length ids == 2
                           then 1 /= abs (head ids - last ids)
                           else 1 < length ids
  | otherwise = 1 < length (elemIndices pair pairs)

chunk3 :: String -> [(Char, Char, Char)]
chunk3 s = zip3 s (tail s) (tail $ tail s)

hasRepeatedLetterWithSeparator :: String -> Bool
hasRepeatedLetterWithSeparator s = True `elem` map isRepeatedLetterWithSeparator (chunk3 s)

isRepeatedLetterWithSeparator :: (Char, Char, Char) -> Bool
isRepeatedLetterWithSeparator (h, _, t) = h == t

isNiceString' :: String -> Bool
isNiceString' s = hasRepeatedPair s && hasRepeatedLetterWithSeparator s

main = do (file:_) <- getArgs
          input <- readFile file
          putStrLn "Number of nice strings under old rules:"
          print $ length (filter isNiceString $ lines input)
          putStrLn "Number of nice strings under new rules:"
          print $ length (filter isNiceString' $ lines input)
