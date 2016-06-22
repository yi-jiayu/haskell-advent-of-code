import System.Environment
import Data.List

main = do (file:_) <- getArgs
          input <- readFile file
          putStrLn "Square feet of wrapping paper the elves should order:"
          print $ sum $ wrapAll $ tokeniseAll input
          putStrLn "Feet of ribbon the elves should order:"
          print $ sum $ tieAll $ tokeniseAll input

wrap :: [String] -> Int
wrap [l', w', h'] = surfaceArea l w h + smallestSide l w h
 where [l, w, h] = map read [l', w', h']

surfaceArea :: Int -> Int -> Int -> Int
surfaceArea l w h = (2 * l * w) + (2 * w * h) + (2 * h * l)

smallestSide :: Int -> Int -> Int -> Int
smallestSide l w h = minimum [l * w, w * h, h * l]

tokeniser :: [String] -> Char -> [String]
tokeniser accum inp
 | inp == 'x' = accum ++ [""]
 | otherwise = init accum ++ [last accum ++ [inp]]

tokenise :: String -> [String]
tokenise = foldl' tokeniser [""]

tokeniseAll :: String -> [[String]]
tokeniseAll input = map tokenise $ lines input

wrapAll = map wrap

shortestPerimeter :: Int -> Int -> Int -> Int
shortestPerimeter l w h = 2 * minimum [l + w, w + h, h + l]

volume :: Int -> Int -> Int -> Int
volume l w h = l * w * h

tie :: [String] -> Int
tie [l', w', h'] = shortestPerimeter l w h + volume l w h
 where [l, w, h] = map read [l', w', h']

tieAll = map tie
