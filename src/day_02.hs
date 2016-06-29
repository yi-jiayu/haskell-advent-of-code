import           Data.List.Split    (splitOn)
import           System.Environment
import           Welcome

wrap :: [String] -> Int
wrap [l', w', h'] = surfaceArea l w h + smallestSide l w h
  where [l, w, h] = map read [l', w', h']
wrap _ = invalid

surfaceArea :: Int -> Int -> Int -> Int
surfaceArea l w h = (2 * l * w) + (2 * w * h) + (2 * h * l)

smallestSide :: Int -> Int -> Int -> Int
smallestSide l w h = minimum [l * w, w * h, h * l]

tokeniseAll :: String -> [[String]]
tokeniseAll input = map (splitOn "x") (lines input)

wrapAll :: [[String]] -> [Int]
wrapAll = map wrap

shortestPerimeter :: Int -> Int -> Int -> Int
shortestPerimeter l w h = 2 * minimum [l + w, w + h, h + l]

volume :: Int -> Int -> Int -> Int
volume l w h = l * w * h

tie :: [String] -> Int
tie [l', w', h'] = shortestPerimeter l w h + volume l w h
  where [l, w, h] = map read [l', w', h']
tie _ = invalid

tieAll :: [[String]] -> [Int]
tieAll = map tie

usage :: IO ()
usage = putStrLn "Usage: day_02.exe path/to/input"

soln :: [String] -> IO ()
soln args = do input <- readFile (head args)
               welcome 2
               putStrLn "Square feet of wrapping paper the elves should order:"
               print $ sum $ wrapAll $ tokeniseAll input
               putStrLn "Feet of ribbon the elves should order:"
               print $ sum $ tieAll $ tokeniseAll input

main = do args <- getArgs
          if null args
            then usage
            else soln args
