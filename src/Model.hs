{-# OPTIONS_GHC -Wno-missing-fields #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.IO

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowGame    GameState

class HasBulletCoolDown a where
  getBulletCoolDown :: a -> Int
  setBulletCoolDown :: a -> Int -> a

decrementBulletCoolDown :: HasBulletCoolDown a => a -> a
decrementBulletCoolDown entity = setBulletCoolDown entity (max 0 (getBulletCoolDown entity - 1))

instance HasBulletCoolDown SpaceShip where
  getBulletCoolDown = bulletCoolDown
  setBulletCoolDown ship cooldown = ship { bulletCoolDown = cooldown }

instance HasBulletCoolDown Enemy where
  getBulletCoolDown = bulletCoolDownEn
  setBulletCoolDown enemy cooldown = enemy { bulletCoolDownEn = cooldown }

data SpaceShip = SpaceShip { spaceShipPos :: Position, bulletCoolDown :: Int }
data Enemy = Enemy { enemyPos :: Position, enemySpeed :: Velocity, bulletCoolDownEn :: Int }
data Asteroid = Asteroid { asteroidPos :: Position, asteroidSpeed :: Velocity }

type Position = (Float, Float)
type Velocity = (Int, Int)

data Bullet = Bullet { bulletPos :: Position, bulletVelocity :: Velocity}


data Explosion = Explosion { explosionPos :: Position, explosionDuration :: Int }

data Direction = UpDir | DownDir | NoDir

data Health = ThreeHealth | TwoHealth | OneHealth | ZeroHealth
    deriving (Eq, Show)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 30

data GameState = GameState {
                   infoToShow          :: InfoToShow
                 , elapsedTime         :: Float
                 , spaceShip           :: SpaceShip
                 , enemies             :: [Enemy]
                 , bullets             :: [Bullet]
                 , asteroids           :: [Asteroid]
                 , explosions          :: [Explosion] 
                 , health              :: Health
                 , isPaused            :: Bool
                 , nO_SECS_BETWEEN_ENEMIES :: Float
                 , gameOver            :: Bool
                 , wKeyPressed         :: Bool
                 , sKeyPressed         :: Bool
                 , score               :: Int
                 , scoreTimer          :: Float
                 , highestScore        :: Int
                 }

initialState :: IO GameState
initialState = do
  highestScore <- readHighestScore "src/highest_score.txt"
  return GameState {
    infoToShow = ShowNothing,
    elapsedTime = 0,
    spaceShip = SpaceShip (-350, 0) 2,
    enemies = [],
    bullets = [],
    asteroids = [],
    explosions = [],
    isPaused = False,
    health = ThreeHealth,
    nO_SECS_BETWEEN_ENEMIES = 5,
    gameOver = False,
    wKeyPressed = False,
    sKeyPressed = False,
    score = 0,
    scoreTimer = 0,
    highestScore = highestScore
  }

readHighestScore :: FilePath -> IO Int
readHighestScore path = do
  contents <- readFile path
  return $ case reads contents of
    [(n, "")] -> n
    _         -> 0
