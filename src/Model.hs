
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K



data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           ,speed ::Int
                           , winx :: Int 
                           , winy :: Int}
  deriving (Show)



initGameState :: Int ->Int ->Int -> Int -> GameState
initGameState vx vy wx wy = GameState vx vy 4 wx wy

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ sp _ _) = gs { persoX = px + sp }

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ sp wx _) = gs { persoX = px - sp }
                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py sp _ _) = gs { persoY = py + sp }

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py sp _ wy) = gs { persoY = py - sp }

collision :: GameState -> Bool
collision gs@(GameState px py _ cx cy) = px < cx + 64 
                                      && px + 100 > cx 
                                      && py < cy + 64 
                                      && 100 + py > cy

genVirus :: GameState -> Int -> Int -> GameState
genVirus gs@(GameState px py sp _ _) x y = GameState px py sp x y

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeZ kbd) && (K.keypressed KeycodeQ kbd) = gstate
                              | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeS kbd) && (K.keypressed KeycodeQ kbd) = gstate
                              | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeZ kbd) = (moveUp (moveRight gstate))
                              | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeS kbd) = (moveDown (moveRight gstate))
                              | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeZ kbd) = (moveUp (moveLeft gstate))
                              | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeS kbd) = (moveDown (moveLeft gstate))
                              | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeD kbd) = gstate
                              | (K.keypressed KeycodeZ kbd) && (K.keypressed KeycodeS kbd) = gstate
                              | (K.keypressed KeycodeD kbd) = (moveRight gstate)
                              | (K.keypressed KeycodeQ kbd) = (moveLeft gstate)
                              | (K.keypressed KeycodeZ kbd) = (moveUp gstate)
                              | (K.keypressed KeycodeS kbd) = (moveDown gstate)
                              | otherwise = gstate


