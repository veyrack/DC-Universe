
module Model where

import SDL

import Data.Map
import qualified Data.Map.Strict as Map

import Carte

import Foreign.C.Types (CInt (..) )

import Keyboard (Keyboard)
import qualified Keyboard as K



data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           ,speed ::Int
                           ,carte::Map Coord Case
                           , winx :: Int 
                           , winy :: Int}
  deriving (Show)



initGameState :: Int ->Int ->Int -> Int-> Map Coord Case -> GameState
initGameState vx vy wx wy carte = GameState vx vy 4 carte wx wy

refreshMap:: GameState -> Map Coord Case -> GameState
refreshMap gs@(GameState px py sp _ _ _) c = gs {persoX = px, persoY=py, speed=sp, carte=c}

--A chaque déplacement du personnage, les coordonnées de la map change
moveLeft :: GameState -> Map Coord Case -> GameState
moveLeft gs@(GameState px py sp _ _ _) c = if (collision gs (((70- (fromIntegral px))-4)`div`20) ((70- (fromIntegral py))`div`20)) then gs else gs { persoX = px + sp, carte = c}

moveRight :: GameState -> Map Coord Case -> GameState
moveRight gs@(GameState px py sp _ wx _ ) c = if (collision gs (((70- (fromIntegral px))+24)`div`20) ((70- (fromIntegral py))`div`20)) then gs else gs { persoX = px - sp , carte = c}
                              
moveUp :: GameState-> Map Coord Case -> GameState
moveUp gs@(GameState px py sp _ _ _ ) c = if (collision gs ((70- (fromIntegral px))`div`20) (((70- (fromIntegral py))-4)`div`20)) then gs else gs { persoY = py + sp , carte = c}

moveDown :: GameState -> Map Coord Case -> GameState
moveDown gs@(GameState px py sp _ _ wy ) c = if (collision gs ((70- (fromIntegral px))`div`20) (((70- (fromIntegral py))+44)`div`20)) then gs else gs { persoY = py - sp , carte = c}

collision :: GameState ->CInt -> CInt -> Bool
collision gs@(GameState px py _ c wx wy) x y = (case (Map.lookup (Coord x y) c) of
                                                Just Mur -> True
                                                Just Entree -> False
                                                Just Sortie -> False
                                                Nothing -> False)
--Verifie la collision pour une tile (car tile=20px et le perso se déplace à 4pixel)
--posx commence à persoX-20
--test move up
checkTiles:: GameState ->CInt -> CInt ->CInt -> CInt -> Bool
checkTiles gs posx posy finx finy
                              | posx==finx = True
                              | collision gs posx posy ==False = False
                              | otherwise = checkTiles gs (posx+4) posy finx finy
                              
basictest:: GameState -> CInt -> CInt -> Bool
basictest gs px py = 70 < px + 20 && 70 + 20 > px && 70 < py + 20 && 20 + 70 > py


collision2 :: GameState -> IO ()
collision2 gs@(GameState px py _ c wx wy) = do
  let value= (Map.lookup (Coord ((70- (fromIntegral px))`div`20) ((70- (fromIntegral py))`div`20) ) c)
  print ("--------", Coord (70-(fromIntegral px)) (70-(fromIntegral py)), value, "-----------")



gameStep :: RealFrac a => GameState -> Keyboard -> a -> Map Coord Case -> GameState
gameStep gstate kbd deltaTime carte | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeZ kbd) && (K.keypressed KeycodeQ kbd) = gstate
                                    | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeS kbd) && (K.keypressed KeycodeQ kbd) = gstate
                                    | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeZ kbd) = (moveUp (moveRight gstate carte) carte)
                                    | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeS kbd) = (moveDown (moveRight gstate carte) carte)
                                    | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeZ kbd) = (moveUp (moveLeft gstate carte) carte)
                                    | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeS kbd) = (moveDown (moveLeft gstate carte) carte)
                                    | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeD kbd) = gstate
                                    | (K.keypressed KeycodeZ kbd) && (K.keypressed KeycodeS kbd) = gstate
                                    | (K.keypressed KeycodeD kbd) = (moveRight gstate carte)
                                    | (K.keypressed KeycodeQ kbd) = (moveLeft gstate carte)
                                    | (K.keypressed KeycodeZ kbd) = (moveUp gstate carte)
                                    | (K.keypressed KeycodeS kbd) = (moveDown gstate carte)
                                    | otherwise = gstate


