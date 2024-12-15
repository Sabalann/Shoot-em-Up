--  This module defines how the state changes
--   in response to time and user input
{-# language NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


enemyRemovalThreshold :: Float
enemyRemovalThreshold = -400 


checkCollision :: Position -> Position -> Float -> Bool
checkCollision (x1, y1) (x2, y2) threshold = distance (x1, y1) (x2, y2) < threshold
  where
    distance (a, b) (c, d) = sqrt ((c - a) ^ 2 + (d - b) ^ 2)

-- | Handle one iteration of the game
-- | Main game step function, processes game state updates per frame
step :: Float -> GameState -> IO GameState
step secs gstate
  | isPaused gstate || gameOver gstate = return gstate  
  | any (\bullet -> checkCollision (bulletPos bullet) (spaceShipPos (spaceShip gstate)) 10) (bullets gstate) = do
      let depletedGstate = depleteHealth gstate
      let gstateAfterImpact = bulletImpact depletedGstate
      gameOverHandler gstateAfterImpact
  | any (\asteroid -> checkCollision (asteroidPos asteroid) (spaceShipPos (spaceShip gstate)) 20) (asteroids gstate) = gameOverHandler gstate
  | any (\enemy -> checkCollision (enemyPos enemy) (spaceShipPos (spaceShip gstate)) 15) (enemies gstate) = gameOverHandler gstate
  | otherwise = do
      let newElapsedTime = elapsedTime gstate + secs
          newScoreTimer = scoreTimer gstate + secs
          (newScore, resetScoreTimer) = if newScoreTimer >= 1
                                        then (score gstate + 1, newScoreTimer - 1)
                                        else (score gstate, newScoreTimer)
          newHighestScore = if newScore > highestScore gstate
                            then newScore
                            else highestScore gstate
          enemySpawnInterval = nO_SECS_BETWEEN_ENEMIES gstate
          playerPos = spaceShipPos (spaceShip gstate)

      (newEnemies, resetElapsedTime) <- if newElapsedTime > enemySpawnInterval
                                        then do
                                          newEnemies <- spawnEnemy (enemies gstate)
                                          return (newEnemies, newElapsedTime - enemySpawnInterval)
                                        else return (enemies gstate, newElapsedTime)
      
      let (newBullets, updatedEnemies) = spawnBullets newEnemies playerPos (bullets gstate)
          movedSpaceShip = moveSpaceShipContinuous gstate

      let gstateWithExplosions = addExplosions gstate { enemies = updatedEnemies, bullets = newBullets }

      let gstateWithUpdatedExplosions = updateExplosions gstateWithExplosions

      return $ gstateWithUpdatedExplosions
        { elapsedTime = resetElapsedTime
        , enemies = map decrementBulletCoolDown (moveEnemies (enemies gstateWithExplosions) newBullets)
        , bullets = moveBullets newBullets
        , asteroids = moveAsteroids (asteroids gstate) newBullets
        , spaceShip = decrementBulletCoolDown (spaceShip movedSpaceShip) 
        , score = newScore
        , scoreTimer = resetScoreTimer
        , highestScore = newHighestScore
        , infoToShow = ShowGame gstate
        }


input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'w') Down _ _) gstate = gstate { wKeyPressed = True }
inputKey (EventKey (Char 'w') Up _ _) gstate = gstate { wKeyPressed = False }
inputKey (EventKey (Char 's') Down _ _) gstate = gstate { sKeyPressed = True }
inputKey (EventKey (Char 's') Up _ _) gstate = gstate { sKeyPressed = False }
inputKey (EventKey (Char 'p') Down _ _) gstate = gstate { isPaused = not (isPaused gstate) } 
inputKey (EventKey (Char 'k') Down _ _) gstate = gstate { nO_SECS_BETWEEN_ENEMIES = nO_SECS_BETWEEN_ENEMIES gstate + 1 } 
inputKey (EventKey (Char 'j') Down _ _) gstate = gstate { nO_SECS_BETWEEN_ENEMIES = max 1 (nO_SECS_BETWEEN_ENEMIES gstate - 1) } 
inputKey (EventKey (Char 'l') Down _ _) gstate = shootBullet gstate 
inputKey _ gstate = gstate 



moveSpaceShipContinuous :: GameState -> GameState
moveSpaceShipContinuous gstate =
  let (x, y) = spaceShipPos (spaceShip gstate)
      newPos = if wKeyPressed gstate
               then (x, y + 5)
               else if sKeyPressed gstate
               then (x, y - 5)
               else (x, y)
  in gstate { spaceShip = (spaceShip gstate) { spaceShipPos = newPos } }

moveAsteroid :: Asteroid -> Asteroid
moveAsteroid asteroid = asteroid { asteroidPos = (x - 1, y) }
  where (x, y) = asteroidPos asteroid

moveAsteroids :: [Asteroid] -> [Bullet] -> [Asteroid]
moveAsteroids asteroids bullets = filter (not . isHit) $ map moveAsteroid asteroids
  where
    isHit asteroid = any (\bullet -> checkCollision (bulletPos bullet) (asteroidPos asteroid) 10) bullets

shootBullet :: GameState -> GameState
shootBullet gstate
  | bulletCoolDown (spaceShip gstate) > 0 = gstate 
  | otherwise = gstate { bullets = newBullet : bullets gstate, spaceShip = (spaceShip gstate) { bulletCoolDown = 20 } } 
  where
    (x, y) = spaceShipPos (spaceShip gstate)
    newBullet = Bullet (x + 10, y) (10, 0)

moveEnemies :: [Enemy] -> [Bullet] -> [Enemy]
moveEnemies enemies bullets = filter (not . isHit) $ map (decrementBulletCoolDown . moveEnemyLeft) enemies
  where
    isHit enemy = any (\bullet -> checkCollision (bulletPos bullet) (enemyPos enemy) 10) bullets

moveEnemyLeft :: Enemy -> Enemy
moveEnemyLeft enemy = enemy { enemyPos = (x - 1, y) }
  where (x, y) = enemyPos enemy


moveBullets :: [Bullet] -> [Bullet]
moveBullets = map moveBullet

moveBullet :: Bullet -> Bullet
moveBullet bullet = bullet { bulletPos = (x + fromIntegral vx, y + fromIntegral vy) }
  where
    (x, y) = bulletPos bullet
    (vx, vy) = bulletVelocity bullet

spawnEnemy :: [Enemy] -> IO [Enemy]
spawnEnemy enemies = do
  randomX <- randomRIO (400, 600) 
  randomY <- randomRIO (-300, 300)
  let newEnemy = Enemy (randomX, randomY) (2, 2) 500
  return (newEnemy : enemies)

spawnBullets :: [Enemy] -> Position -> [Bullet] -> ([Bullet], [Enemy])
spawnBullets enemies playerPos bullets = (bullets ++ concat newBullets, updatedEnemies)
  where
    (newBullets, updatedEnemies) = unzip [if bulletCoolDownEn enemy <= 0 && fst (enemyPos enemy) > fst playerPos + 100
                                          then ([Bullet (x - 12, y) (calculateBulletVelocity (x + 12, y) playerPos)], enemy { bulletCoolDownEn = 500 }) 
                                          else ([], enemy)
                                          | enemy <- enemies
                                          , let (x, y) = enemyPos enemy]

calculateBulletVelocity :: Position -> Position -> Velocity
calculateBulletVelocity (ex, ey) (px, py) = (vx, vy)
  where
    dx = px - ex
    dy = py - ey
    magnitude = sqrt (dx * dx + dy * dy)
    speed = 2
    vx = round (dx / magnitude * speed)
    vy = round (dy / magnitude * speed)


bulletImpact :: GameState -> GameState
bulletImpact gstate = 
  let
      remainingBullets = filter (not . checkCollisionWithSpaceShip) (bullets gstate)
  in gstate { bullets = remainingBullets }
  where
    checkCollisionWithSpaceShip bullet = checkCollision (bulletPos bullet) (spaceShipPos (spaceShip gstate)) 10


depleteHealth :: GameState -> GameState
depleteHealth gstate =
    case health gstate of
        ThreeHealth -> gstate { health = TwoHealth }
        TwoHealth -> gstate { health = OneHealth }
        OneHealth -> gstate { health = ZeroHealth }
        ZeroHealth -> gstate 

addExplosions :: GameState -> GameState
addExplosions gstate =
    gstate { explosions = newExplosions ++ explosions gstate }
  where
    newExplosions = [ Explosion { explosionPos = enemyPos enemy, explosionDuration = 30 }
                    | enemy <- enemies gstate, isHitByBullet enemy (bullets gstate)
                    ]

    isHitByBullet enemy = any (\bullet -> checkCollision (enemyPos enemy) (bulletPos bullet) 10)


updateExplosions :: GameState -> GameState
updateExplosions gstate =
    gstate { explosions = [ explosion { explosionDuration = explosionDuration explosion - 1 }
                          | explosion <- explosions gstate
                          , explosionDuration explosion > 1
                          ]
           }



updateHighestScore :: FilePath -> Int -> IO ()
updateHighestScore path score = do
  putStrLn $ "Updating highest score to: " ++ show score
  writeFile path (show score)


gameOverHandler :: GameState -> IO GameState
gameOverHandler gstate = do
  let currentScore = score gstate
      highestScorePath = "src/highest_score.txt"


  highestScore <- readHighestScore highestScorePath


  if currentScore > highestScore
    then updateHighestScore highestScorePath currentScore
    else return ()

  if health gstate == ZeroHealth
    then return gstate { gameOver = True, highestScore = max currentScore highestScore }
    else return (depleteHealth gstate)  