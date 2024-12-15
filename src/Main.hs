module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
  gstate <- initialState  -- Execute initialState to get the GameState
  playIO
    (InWindow "Space Shooter" (800, 800) (50, 50))   -- Window configuration
    (greyN 0.1) 60                                     -- Background color and FPS
    gstate                                            -- Pass the actual GameState
    view                                              -- View function
    input                                             -- Input handling
    step                                              -- Game step update