import           Data.List
import           Data.List.Split    (splitOn)
import           System.Environment

combinations k ns = filter ((k==).length) (subsequences ns)

data Item = Item { cost :: Int
                 , dmg  :: Int
                 , amr  :: Int } deriving (Show)

data Attacker = Attacker { hp    :: Int
                         , hit   :: Int
                         , rst   :: Int
                         , worth :: Int} deriving (Show)

combine :: Item -> Item -> Item
combine i1 i2 = Item (cost i1 + cost i2) (dmg i1 + dmg i2) (amr i1 + amr i2)

eqp :: Attacker -> [Item] -> Attacker
eqp a [] = a
eqp a items = foldl' eqp1 a items

eqp1 :: Attacker -> Item -> Attacker
eqp1 a i = Attacker (hp a) (hit a + dmg i) (rst a + amr i) (worth a + cost i)

simulate :: Attacker -> Attacker -> (Attacker, Attacker)
simulate chal def = let def' = chal `attack` def; chal' = def `attack` chal
               in if hp def' < 1 then (chal, def')
                  else if hp chal' < 1 then (chal', def')
                  else simulate chal' def'

doesChallengerWin :: (Attacker, Attacker) -> Bool
doesChallengerWin result = hp (snd result) < 1

battle :: Attacker -> Attacker -> (Bool, (Attacker, Attacker))
battle boss player = let outcome = simulate player boss
                     in (doesChallengerWin outcome, outcome)

attack :: Attacker -> Attacker -> Attacker
attack a d = Attacker (hp d - max 1 (hit a - rst d)) (hit d) (rst d) (worth d)

types :: String -> [String]
types = splitOn "\n\n"

items :: String -> [[String]]
items s = tail $ map words (lines s)

parseItem :: [String] -> Item
parseItem (_:cost':dmg':amr':_) = let [cost, dmg, amr] = map read [cost', dmg', amr']
                                   in Item cost dmg amr

parseRing :: [String] -> [String]
parseRing (name:modifier:xs) = (name ++ modifier):xs

parseItems :: [[[String]]] -> [[Item]]
parseItems (weapons:armour:rings:_) = map (map parseItem) [weapons, armour, map parseRing rings]

combineRings :: [Item] -> Item
combineRings (r1:r2:_) = combine r1 r2

itemChoices :: [[Item]] -> [[Item]]
itemChoices (weapons:armours:rings:_) = sequence [weapons, Item 0 0 0:armours, map combineRings (combinations 2 (Item 0 0 0:Item 0 0 0:rings))]

playerLoadouts :: Int -> [[Item]] -> [Attacker]
playerLoadouts hp = map $ eqp (Attacker hp 0 0 0)

battles :: [Attacker] -> Attacker -> [(Bool, (Attacker, Attacker))]
battles loadouts boss = map (battle boss) loadouts

main = do (inpFile:playerHp':bossHp':bossDmg':bossAmr':_) <- getArgs
          input <- readFile inpFile
          let [playerHp, bossHp, bossDmg, bossAmr] = map read [playerHp', bossHp', bossDmg', bossAmr']
          let choices = itemChoices $ parseItems $ map items (types input)
          let outcomes = battles (playerLoadouts playerHp choices) (Attacker bossHp bossDmg bossAmr 0)
          let winningOutcomes = filter fst outcomes
          let cheapestWinningLoadout = minimum $ map (worth . fst . snd) winningOutcomes
          putStrLn "Least amount of gold player can spend and still win:"
          print cheapestWinningLoadout
          let losingOutcomes = filter (not . fst) outcomes
          let costliestLosingLoadout = maximum $ map (worth . fst . snd) losingOutcomes
          putStrLn "Most amount of gold player can spend and still lose:"
          print costliestLosingLoadout
