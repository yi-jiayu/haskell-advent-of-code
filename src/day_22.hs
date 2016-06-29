import           System.Environment
import           Welcome

data GameState = GameState { playerHp          :: Int
                           , playerMp          :: Int
                           , playerAmr         :: Int
                           , bossHp            :: Int
                           , bossDmg           :: Int
                           , shieldTurnsLeft   :: Int
                           , poisonTurnsLeft   :: Int
                           , rechargeTurnsLeft :: Int
                           , mpUsed            :: Int
                           , hardMode          :: Bool
                           , outcome           :: Result } deriving (Show)

data Spell = MISSILE | DRAIN | SHIELD | POISON | RECHARGE deriving (Enum, Show, Eq)

data Result = WIN | LOSE | UNDET deriving (Show, Eq)

missileCost = 53
missileDmg = 4

drainCost = 73
drainDmg = 2
drainHeal = 2

shieldCost = 113
shieldDuration = 6
shieldArmour = 7

poisonCost = 173
poisonDuration = 6
poisonDmg = 3

rechargeCost = 229
rechargeDuration = 5
rechargeAmount = 101

newGame :: Int -> Int -> Int -> Int -> Bool -> GameState
newGame pHp pMp bHp bDmg hm = GameState pHp pMp 0 bHp bDmg 0 0 0 0 hm UNDET

dfs :: Int -> [GameState] -> Int
dfs best [] = best
dfs best stack = let (best', stack') = visit best (last stack) (init stack)
                       in dfs best' stack'

visit :: Int -> GameState-> [GameState] -> (Int, [GameState])
visit best state stack
  | mpUsed state >= best = (best, stack)
  | outcome state == WIN = let best' = mpUsed state
                           in (best', stack)
  | outcome state == LOSE = (best, stack)
  | playerMp state < missileCost = (best, stack) -- not having any enough mp to cast any spell is the same as losing
  -- otherwise, push all the possible outcomes from this turn onto the stack
  | otherwise = let stack' = stack ++ [turn RECHARGE state | (playerMp state >= rechargeCost) && (rechargeTurnsLeft state <= 1)]
                                   ++ [turn POISON state | (playerMp state >= poisonCost) && (poisonTurnsLeft state <= 1)]
                                   ++ [turn SHIELD state | (playerMp state >= shieldCost) && (shieldTurnsLeft state <= 1)]
                                   ++ [turn DRAIN state | playerMp state >= drainCost]
                                   ++ [turn MISSILE state | playerMp state >= missileCost]
                in (best, stack')

turn :: Spell -> GameState -> GameState
turn spell state = let hardmodeCheck = applyHardmode state
                   in if playerHp hardmodeCheck <= 0
                   then hardmodeCheck { outcome = LOSE }
                   else let preCast = applyEffects hardmodeCheck -- apply effects before player's turn
                        in if bossHp preCast <= 0
                           then preCast { outcome = WIN }
                           else let postCast = cast spell preCast -- cast spell
                                in if bossHp postCast <= 0
                                   then postCast { outcome = WIN }
                                   else let preBossAtk = applyEffects postCast -- apply effects before boss's turn
                                        in if bossHp preBossAtk <= 0
                                           then preBossAtk { outcome = WIN }
                                           else let postBossAtk = bossAttack preBossAtk -- boss attacks
                                                in if playerHp postBossAtk <= 0
                                                   then postBossAtk { outcome = LOSE }
                                                   else postBossAtk { outcome = UNDET }

bossAttack :: GameState -> GameState
bossAttack state = state { playerHp = playerHp state - max 1 (bossDmg state - playerAmr state) }

cast :: Spell -> GameState -> GameState
cast spell gameState = case spell of MISSILE -> castMissile gameState
                                     DRAIN -> castDrain gameState
                                     SHIELD -> castShield gameState
                                     POISON -> castPoison gameState
                                     RECHARGE -> castRecharge gameState

castMissile :: GameState -> GameState
castMissile gameState = gameState { bossHp = bossHp gameState - missileDmg
                                  , playerMp = playerMp gameState - missileCost
                                  , mpUsed = mpUsed gameState + missileCost }

castDrain :: GameState -> GameState
castDrain gameState = gameState { bossHp = bossHp gameState - drainDmg
                                , playerHp = playerHp gameState + drainHeal
                                , playerMp = playerMp gameState - drainCost
                                , mpUsed = mpUsed gameState + drainCost }

castShield :: GameState -> GameState
castShield gameState = gameState { playerMp = playerMp gameState - shieldCost
                                 , shieldTurnsLeft = shieldDuration
                                 , mpUsed = mpUsed gameState + shieldCost }

castPoison :: GameState -> GameState
castPoison gameState = gameState { playerMp = playerMp gameState - poisonCost
                                 , poisonTurnsLeft = poisonDuration
                                 , mpUsed = mpUsed gameState + poisonCost }

castRecharge :: GameState -> GameState
castRecharge gameState = gameState { playerMp = playerMp gameState - rechargeCost
                                   , rechargeTurnsLeft = rechargeDuration
                                   , mpUsed = mpUsed gameState + rechargeCost }

applyEffects :: GameState -> GameState
applyEffects = applyShield . applyPoison . applyRecharge

applyShield :: GameState -> GameState
applyShield gameState = if shieldTurnsLeft gameState > 0
                        then gameState { playerAmr = shieldArmour
                                       , shieldTurnsLeft = shieldTurnsLeft gameState - 1 }
                        else gameState { playerAmr = 0 }

applyPoison :: GameState -> GameState
applyPoison gameState = if poisonTurnsLeft gameState > 0
                        then gameState { bossHp = bossHp gameState - poisonDmg
                                       , poisonTurnsLeft = poisonTurnsLeft gameState - 1 }
                        else gameState

applyRecharge :: GameState -> GameState
applyRecharge gameState = if rechargeTurnsLeft gameState > 0
                          then gameState { playerMp = playerMp gameState + rechargeAmount
                                         , rechargeTurnsLeft = rechargeTurnsLeft gameState - 1 }
                          else gameState

applyHardmode :: GameState -> GameState
applyHardmode state = if hardMode state
                      then state { playerHp = playerHp state - 1 }
                      else state

usage :: IO ()
usage = putStrLn "Usage: day_22.exe player_hp player_mp boss_hp boss_dmg"

soln :: [String] -> IO ()
soln args = do let (pHp: pMp: bHp: bDmg: _) = readInts args
               welcome 22
               putStrLn "Least amount of mana needed to win (non-hardmode):"
               print $ dfs (maxBound :: Int) [newGame pHp pMp bHp bDmg False]
               putStrLn "Least amount of mana needed to win (hardmode):"
               print $ dfs (maxBound :: Int) [newGame pHp pMp bHp bDmg True]

main = do args <- getArgs
          if null args
            then usage
            else soln args
