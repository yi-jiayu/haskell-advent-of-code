import           Data.List
import           System.Environment
import           Welcome

readInts :: [String] -> [Int]
readInts = map read

calculateQE :: [Int] -> Int
calculateQE = product

findTarget :: [Int] -> Int
findTarget pkgs = sum pkgs `quot` 3

findTarget4 :: [Int] -> Int
findTarget4 pkgs = sum pkgs `quot` 4

pack1 :: [Int] -> Int -> [[Int]]
pack1 pkgs target = filter (\x -> sum x == target) (subsequences pkgs)

findPackings :: [Int] -> Int -> [[Int]]
findPackings pkgs target = filter (verifyPacking3 pkgs target) (pack1 pkgs target)

verifyPacking3 :: [Int] -> Int -> [Int] -> Bool
verifyPacking3 pkgs target packing = let remaining = pkgs \\ packing
                                    in (not . null) (pack1 remaining target)

verifyPacking4 :: [Int] -> Int -> [Int] -> Bool
verifyPacking4 pkgs target packing = let remaining3 = pkgs \\ packing
                                         packings3 = findPackings remaining3 target
                                     in any (verifyPacking3 remaining3 target) packings3

findPacking :: [Int] -> Int -> [Int]
findPacking pkgs target = let packings = findPackings pkgs target
                          in head $ filter (\x -> verifyPacking3 pkgs target x && not (findLowerQE x packings)) packings

findPacking4 :: [Int] -> Int -> [Int]
findPacking4 pkgs target = let packings = findPackings pkgs target
                           in head $ filter (\x -> verifyPacking4 pkgs target x && not (findLowerQE x packings)) packings

findLowerQE :: [Int] -> [[Int]] -> Bool
findLowerQE packing packings = any (\x -> calculateQE x < calculateQE packing) (takeWhile (\x -> length x <= length packing) packings)

main = do (inpFile:_) <- getArgs
          input <- readFile inpFile
          let pkgs' = (reverse . readInts . lines) input
          let pkgs = if last pkgs' == 1 then 1:init pkgs'
                     else pkgs'
          let target = findTarget pkgs
          let packing = findPacking pkgs target
          welcome 24
          putStrLn "Minimum quantum entanglement for the first group out of three:"
          print $ calculateQE packing
          let target' = findTarget4 pkgs
          let packing' = findPacking4 pkgs target'
          putStrLn "Minimum quantum entanglement for the first group out of four:"
          print $ calculateQE packing'
