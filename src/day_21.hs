import           Data.List
import           System.Environment
import           Welcome

combinations k ns = filter ((k==).length) (subsequences ns)

data Item = Item { cost :: Int
                 , dmg  :: Int
                 , amr  :: Int } deriving (Show)

data Attacker = Attacker { hp    :: Int
                         , hit   :: Int
                         , rst   :: Int
                         , worth :: Int} deriving (Show)

weapons :: [Item]
weapons = [ Item  8  4  0
          , Item 10  5  0
          , Item 25  6  0
          , Item 40  7  0
          , Item 74  8  0 ]

armours :: [Item]
armours = [ Item   0  0  0
          , Item  13  0  1
          , Item  31  0  2
          , Item  53  0  3
          , Item  75  0  4
          , Item 102  0  5 ]

rings :: [Item]
rings = [ Item   0  0  0
        , Item  25  1  0
        , Item  50  2  0
        , Item 100  3  0
        , Item  20  0  1
        , Item  40  0  2
        , Item  80  0  3 ]

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

-- Wrote this code to parse the item shop inventory, but decided to hardcode it
-- since the item shop seems to be a constant
{-
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
-}

combineRings :: [Item] -> Item
combineRings (r1:r2:_) = combine r1 r2

itemChoices :: [[Item]]
itemChoices = sequence [weapons, armours, map combineRings (combinations 2 rings)]

playerLoadouts :: Int -> [[Item]] -> [Attacker]
playerLoadouts hp = map $ eqp (Attacker hp 0 0 0)

battles :: [Attacker] -> Attacker -> [(Bool, (Attacker, Attacker))]
battles loadouts boss = map (battle boss) loadouts

usage :: IO ()
usage = putStrLn "Usage: day_21.exe player_hp boss_hp boss_dmg boss_amr"

soln :: [String] -> IO ()
soln args = do let (playerHp':bossHp':bossDmg':bossAmr':_) = args
               let [playerHp, bossHp, bossDmg, bossAmr] = readInts [playerHp', bossHp', bossDmg', bossAmr']
               let outcomes = battles (playerLoadouts playerHp itemChoices) (Attacker bossHp bossDmg bossAmr 0)
               let (winningOutcomes, losingOutcomes) = partition fst outcomes
               let cheapestWinningLoadout = minimum (map (worth . fst . snd) winningOutcomes)
               let costliestLosingLoadout = maximum (map (worth . fst . snd) losingOutcomes)
               welcome 21
               putStrLn "Least amount of gold player can spend and still win:"
               print cheapestWinningLoadout
               putStrLn "Most amount of gold player can spend and still lose:"
               print costliestLosingLoadout


main = do args <- getArgs
          if length args < 4 then usage
          else soln args
