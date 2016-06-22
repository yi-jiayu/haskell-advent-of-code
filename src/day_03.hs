import           Data.List
import qualified Data.Set           as Set
import           System.Environment

main :: IO ()
main = do (file:_) <- getArgs
          input <- readFile file
          putStrLn "Houses which receive at least one present in year one:"
          print $ countHouses input (0, 0)
          putStrLn "Houses which receive at least one present in year two:"
          print $ countHouses' input (0, 0)

move :: ((Int, Int), [(Int, Int)]) -> Char -> ((Int, Int), [(Int, Int)])
move state direction
  | direction == '>' = let newpos = (fst (fst state) + 1, snd $ fst state)
                       in (newpos, newpos:snd state)
  | direction == '<' = let newpos = (fst (fst state) - 1, snd $ fst state)
                       in (newpos, newpos:snd state)
  | direction == '^' = let newpos = (fst $ fst state, snd (fst state) + 1)
                       in (newpos, newpos:snd state)
  | direction == 'v' = let newpos = (fst $ fst state, snd (fst state) - 1)
                       in (newpos, newpos:snd state)

countHouses :: String -> (Int, Int) -> Int
countHouses directions startpos = length $ Set.fromList $ snd $ foldl' move (startpos, [startpos]) directions

santapos (pos, _, _) = pos

rsantapos (_, pos, _) = pos

visited (_, _, houses) = houses

move' :: ((Int, Int), (Int, Int), [(Int, Int)]) -> Char -> ((Int, Int), (Int, Int), [(Int, Int)])
move' state direction = if mod (length $ visited state) 2 == 0
                        then moveSanta state direction
                        else moveRoboSanta state direction

moveSanta :: ((Int, Int), (Int, Int), [(Int, Int)]) -> Char -> ((Int, Int), (Int, Int), [(Int, Int)])
moveSanta state direction
  | direction == '>' = let newpos = (fst (santapos state) + 1, snd $ santapos state)
                       in (newpos, rsantapos state, newpos:visited state)
  | direction == '<' = let newpos = (fst (santapos state) - 1, snd $ santapos state)
                       in (newpos, rsantapos state, newpos:visited state)
  | direction == '^' = let newpos = (fst $ santapos state, snd (santapos state) + 1)
                       in (newpos, rsantapos state, newpos:visited state)
  | direction == 'v' = let newpos = (fst $ santapos state, snd (santapos state) - 1)
                       in (newpos, rsantapos state, newpos:visited state)

moveRoboSanta :: ((Int, Int), (Int, Int), [(Int, Int)]) -> Char -> ((Int, Int), (Int, Int), [(Int, Int)])
moveRoboSanta state direction
  | direction == '>' = let newpos = (fst (rsantapos state) + 1, snd $ rsantapos state)
                       in (santapos state, newpos, newpos:visited state)
  | direction == '<' = let newpos = (fst (rsantapos state) - 1, snd $ rsantapos state)
                       in (santapos state, newpos, newpos:visited state)
  | direction == '^' = let newpos = (fst $ rsantapos state, snd (rsantapos state) + 1)
                       in (santapos state, newpos, newpos:visited state)
  | direction == 'v' = let newpos = (fst $ rsantapos state, snd (rsantapos state) - 1)
                       in (santapos state, newpos, newpos:visited state)

countHouses' :: String -> (Int, Int) -> Int
countHouses' directions startpos = length $ Set.fromList $ visited $ foldl' move' (startpos, startpos, [startpos]) directions
