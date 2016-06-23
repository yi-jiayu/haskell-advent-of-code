import System.Environment
import Data.List (findIndex)
import Data.Maybe (fromJust)

factors :: Int -> [Int]
factors n = let f1 = filter ((== 0) . mod n) [2..(floor . sqrt $ fromIntegral n)]
                f2 = map (quot n) f1
            in 1:n:(f1 ++ f2)

presents :: Int -> Int
presents = (* 10) . sum . factors

presents' :: Int -> Int
presents' h = (*11) . sum . filter (> quot h 50). factors $ h

main = do (nPresents:_) <- getArgs
          putStrLn "Haskell Advent of Code"
          putStrLn "https://github.com/yi-jiayu/haskell-advent-of-code"
          putStrLn "Day 20\n"
          putStrLn $ "Lowest house number to get " ++ nPresents ++ " presents:"
          print $ fromJust $ findIndex (>= read nPresents) $ map presents [0..]
          putStrLn $ "Lowest house number to get " ++ nPresents ++ " presents with lazy elves:"
          print $ fromJust $ findIndex (>= read nPresents) $ map presents' [0..]
