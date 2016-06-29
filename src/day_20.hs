import           System.Environment
import           Welcome

factors :: Int -> [Int]
factors n = let f1 = filter ((== 0) . mod n) [2..(floor . sqrt . fromIntegral) n]
                f2 = map (quot n) f1
            in 1:n:(f1 ++ f2)

presents :: Int -> Int
presents = (* 10) . sum . factors

presents' :: Int -> Int
presents' h = (*11) . sum . filter (> quot h 50). factors $ h

usage :: IO ()
usage = putStrLn "Usage: day_20.exe num_presents"

soln :: [String] -> IO ()
soln args = do let nPresents' = head args
               let nPresents = read (head args) :: Int
               welcome 20
               putStrLn $ "Lowest house number to get " ++ nPresents' ++ " presents:"
               print $ (length . takeWhile (< nPresents) . map presents) [0..]
               putStrLn $ "Lowest house number to get " ++ nPresents' ++ " presents with lazy elves:"
               print $ (length . takeWhile (< nPresents) . map presents') [0..]


main = do args <- getArgs
          if null args then usage
          else soln args
