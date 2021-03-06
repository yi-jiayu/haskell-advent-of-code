import           System.Environment
import           Welcome

data Code = Code { row :: Int
                 , col :: Int
                 , val :: Int } deriving (Show)

startVal = 20151125
multFactor = 252533
divFactor = 33554393

next :: Code -> Code
next code = let row' = if row code == 1 then col code + 1 else row code - 1
                col' = if row code == 1 then 1 else col code + 1
                val' = (val code * multFactor) `rem` divFactor
            in Code row' col' val'

codeAt ::Int -> Int -> Code -> Code
codeAt r c code = if (row code == r) && (col code == c) then code
                  else codeAt r c (next code)

usage :: IO ()
usage = putStrLn "Usage: day_25.exe row col"

soln :: [String] -> IO ()
soln args =  do let (targetRow:targetCol:_) = readInts args
                let code = Code 1 1 startVal
                welcome 25
                putStrLn "Code for the machine:"
                print $ val (codeAt targetRow targetCol code)

main = do args <- getArgs
          if null args then usage
          else soln args
