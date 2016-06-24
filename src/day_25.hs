import System.Environment

data Code = Code { row :: Int
                 , col :: Int
                 , val :: Int } deriving (Show)

readInts :: [String] -> [Int]
readInts = map read

next' :: Int -> Int -> Code -> Code
next' mult divr code = let row' = if row code == 1 then col code + 1 else row code - 1
                           col' = if row code == 1 then 1 else col code + 1
                           val' = (val code * mult) `rem` divr
                       in Code row' col' val'

codeAt :: Int -> Int -> (Code -> Code) -> Code -> Code
codeAt r c next code = if (row code == r) && (col code == c) then code
                       else codeAt r c next (next code)

main = do args <- getArgs
          let (startVal:mult:divr:targetRow:targetCol:_) = readInts args
          let code = Code 1 1 startVal
          let next = next' mult divr
          print $ codeAt targetRow targetCol next code
