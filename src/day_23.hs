import           Data.Array
import           System.Environment
import           Welcome

data Regs = Regs { pc :: Int
                 , r1 :: Int
                 , r2 :: Int } deriving (Show)

data Instruction = Instruction { op :: Op
                               , a  :: Int
                               , b  :: Int } deriving (Show)

data Op = HLF | TPL | INC | JMP | JIE | JIO | NOP deriving (Show, Eq)

parse :: String -> Instruction
parse = parse' . words

parse' :: [String] -> Instruction
parse' s@(op':a':_) = case op' of "hlf" -> parse1 s
                                  "tpl" -> parse1 s
                                  "inc" -> parse1 s
                                  "jmp" -> parseJmp a'
                                  "jie" -> parseJmpIf s
                                  "jio" -> parseJmpIf s

parse1 :: [String] -> Instruction
parse1 (op:a':_) = let a = if a' == "a" then 1
                           else 2
                   in case op of "hlf" -> Instruction HLF a 0
                                 "tpl" -> Instruction TPL a 0
                                 "inc" -> Instruction INC a 0

parseJmp :: String -> Instruction
parseJmp offset = let a = (read . tail) offset
                  in if head offset == '+' then Instruction JMP a 0
                     else Instruction JMP (negate a) 0

parseJmpIf :: [String] -> Instruction
parseJmpIf (op':reg':a':_) = let op = if op' == "jie" then JIE
                                      else JIO
                                 reg = if head reg' == 'a' then 1
                                       else 2
                                 a = if head a' == '+' then (read . tail) a'
                                     else (negate . read . tail) a'
                             in Instruction op reg a

parseAll :: [String] -> [Instruction]
parseAll = map parse

load :: [Instruction] -> Array Int Instruction
load ins = listArray (1, length ins) ins

run :: Array Int Instruction -> Regs -> Regs
run prog regs = if pc regs > length prog then regs
                   else let ins = prog ! pc regs
                        in run prog (execute regs ins)


execute :: Regs -> Instruction -> Regs
execute regs ins = case op ins of HLF -> hlf regs a'
                                  TPL -> tpl regs a'
                                  INC -> inc regs a'
                                  JMP -> jmp regs a'
                                  JIE -> jie regs a' b'
                                  JIO -> jio regs a' b'
                                  NOP -> regs
                   where a' = a ins
                         b' = b ins

updateReg :: Regs -> Int -> (Int -> Int) -> Regs
updateReg regs reg func = let pc' = pc regs + 1
                              ra = r1 regs
                              rb = r2 regs
                          in if reg == 1 then Regs pc' (func ra) rb
                             else Regs pc' ra (func rb)

hlf :: Regs -> Int -> Regs
hlf regs a = updateReg regs a (`quot` 2)

tpl :: Regs -> Int -> Regs
tpl regs a = updateReg regs a (* 3)

inc :: Regs -> Int -> Regs
inc regs a = updateReg regs a (+ 1)

jmp :: Regs -> Int -> Regs
jmp regs a = Regs (pc regs + a) (r1 regs) (r2 regs)

jie :: Regs -> Int -> Int -> Regs
jie regs a b
  | (a == 1) && even ra = jmp regs b
  | (a == 2) && even rb = jmp regs b
  | otherwise = jmp regs 1
  where ra = r1 regs
        rb = r2 regs

jio :: Regs -> Int -> Int -> Regs
jio regs a b
  | (a == 1) && (ra == 1) = jmp regs b
  | (a == 2) && (rb == 1) = jmp regs b
  | otherwise = jmp regs 1
  where ra = r1 regs
        rb = r2 regs

usage :: IO ()
usage = putStrLn "day_23.exe path/to/input"

soln :: [String] -> IO ()
soln args = do input <- readFile (head args)
               let prog = load $ parseAll (lines input)
               welcome 23
               putStrLn "Value in register b at end of program:"
               print $ r2 (run prog $ Regs 1 0 0)
               putStrLn "Value in register b at end of program when register a starts at 1:"
               print $ r2 (run prog $ Regs 1 1 0)


main = do args <- getArgs
          if null args then usage
          else soln args
