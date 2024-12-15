-- This module defines how to turn the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

-- | Convert the game state to a picture (pure function)
viewPure :: GameState -> Picture
viewPure gstate = pictures $
  [ case infoToShow gstate of
      ShowNothing   -> blank
      ShowANumber n -> color green (text (show n))
      ShowGame game -> pictures $
        [ 
          translate ((fst (spaceShipPos (spaceShip game)))) (snd (spaceShipPos (spaceShip game))) spaceShipPicture
        ] ++ map drawEnemy (enemies game) ++ map drawBullet (bullets game) ++ [drawScore game, drawHighestScore game]
      ShowAChar   c -> color green (text [c])
  ] ++ [color red (translate (-100) 0 (scale 0.5 0.5 (text "Paused"))) | isPaused gstate]
    ++ [drawHealthDots gstate]
    ++ [color red (translate (-100) 0 (scale 0.5 0.5 (text "Game Over"))) | gameOver gstate]
    ++ map drawExplosion (explosions gstate)

-- | Draw the game elements
drawGame :: GameState -> Picture
drawGame gstate = pictures $
  [ translate x y $ color white $ rectangleSolid 50 50
  | (x, y) <- map spaceShipPos [spaceShip gstate]
  ] ++
  [ translate x y $ color red $ rectangleSolid 50 50
  | (x, y) <- map enemyPos (enemies gstate)
  ] ++
  [ translate x y $ color yellow $ rectangleSolid 10 10
  | (x, y) <- map bulletPos (bullets gstate)
  ] ++
  [ translate x y $ color (greyN 0.5) $ circleSolid 20
  | (x, y) <- map asteroidPos (asteroids gstate)
  ]

-- | Graphics of the objects in the game

spaceShipPicture :: Picture
spaceShipPicture = pictures [rotate 90 (color white (rectangleSolid shipHeight shipWidth)), rotate 90 ( color (greyN 0.4) (rectangleSolid (shipHeight - shipBorder) (shipWidth - shipBorder))) ]

enemyPicture :: Picture
enemyPicture = pictures  [rotate 90 (color red (rectangleSolid shipHeight shipWidth)), rotate 90 ( color (makeColor 0.5 0 0 1) (rectangleSolid (shipHeight - shipBorder) (shipWidth - shipBorder)))]

drawEnemy :: Enemy -> Picture
drawEnemy enemy = translate x y enemyPicture
  where (x, y) = enemyPos enemy

asteroidPicture :: Picture
asteroidPicture = pictures [color (greyN 0.2) (circleSolid 10), color (greyN 0.4) (circle 10) ]

drawHealthDots :: GameState -> Picture
drawHealthDots gstate =
    pictures (case health gstate of
        ThreeHealth -> [drawRedDot (-350), drawRedDot (-330)]
        TwoHealth -> [drawRedDot (-350), drawRedDot (-330)]
        OneHealth -> [drawRedDot (-350)]
        ZeroHealth -> []
    )
    where
        drawRedDot :: Float -> Picture
        drawRedDot x = color green (translate x 380 (rectangleSolid 10 10)) 

shipHeight :: Float
shipHeight = 25

shipWidth :: Float
shipWidth = 50

shipBorder :: Float
shipBorder = 4

bulletPicture :: Picture
bulletPicture = pictures [color (greyN 0.2) (circleSolid 5), color (dark red) (circle 5) ]

drawBullet :: Bullet -> Picture
drawBullet bullet = translate x y bulletPicture
  where (x, y) = bulletPos bullet


drawScore :: GameState -> Picture
drawScore gstate = translate (-300) 200 $ scale 0.3 0.3 $ color white $ text $ "Score: " ++ show (score gstate)


drawHighestScore :: GameState -> Picture
drawHighestScore gstate = translate (-300) 150 $ scale 0.3 0.3 $ color white $ text $ "Highest Score: " ++ show (highestScore gstate)


drawExplosion :: Explosion -> Picture
drawExplosion explosion = translate x y $ color explosionColor $ circleSolid explosionSize
  where
    (x, y) = explosionPos explosion
    explosionSize = fromIntegral (explosionDuration explosion) * 2 
    explosionColor = makeColor 1 0.5 0 1 