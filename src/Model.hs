
module Model where

import SDL

import Data.Map
import qualified Data.Map.Strict as Map

import Carte

import Foreign.C.Types (CInt (..) )

import Keyboard (Keyboard)
import qualified Keyboard as K

--Transx : Correspond au déplacement sur l'axe des x du personnage (En réalité, c'est l'environnement qui se "deplace")
--Transy: Pareil que Transx mais sur l'axe des y

--PersoX: L'axe x où se situe le personnage
--PersoY: L'axe des y où se situe le personnage

data GameState = GameState { transX :: Int
                           , transY :: Int
                           ,speed ::Int
                           , persoX :: CInt 
                           , persoY :: CInt
                           ,carte::Map Coord Case
                           }
  deriving (Show)



initGameState :: Int ->Int ->CInt -> CInt-> Map Coord Case -> GameState
initGameState tx ty px py carte = GameState tx ty 4 px py carte

refreshMap:: GameState -> Map Coord Case -> GameState
refreshMap gs@(GameState tx ty sp px py _) c = gs {transX = tx, transY=ty, speed=sp ,carte=c}

--Deplacement du personnage (Ici, le déplacement du personnage est en réalité une translation de l'environnement, soit un scrolling)
moveLeft :: GameState -> Map Coord Case -> GameState
moveLeft gs@(GameState tx ty sp px py _) c = if (collision gs (((px- (fromIntegral tx))-4)`div`20) ((py- (fromIntegral ty))`div`20)) --collision coin haut gauche
                                              || (collision gs (((px- (fromIntegral tx))-4)`div`20) (((py+45)- (fromIntegral ty))`div`20)) --collision coin bas gauche
                                              || (collision gs (((px- (fromIntegral tx))-4)`div`20) (((py+23)- (fromIntegral ty))`div`20)) --collision milieu gauche
                                              then gs else gs { transX = tx + sp, carte = c}

moveRight :: GameState -> Map Coord Case -> GameState
moveRight gs@(GameState tx ty sp px py _) c = if (collision gs ((((px+25)- (fromIntegral tx))+4)`div`20) (((py+22)- (fromIntegral ty))`div`20)) --collision milieu droit
                                                || (collision gs ((((px+25)- (fromIntegral tx))+4)`div`20) ((py- (fromIntegral ty))`div`20))--collision coin haut droit
                                                || (collision gs ((((px+25)- (fromIntegral tx))+4)`div`20) (((py+45)- (fromIntegral ty))`div`20)) --collision coin bas droit
                                                then gs else gs { transX = tx - sp , carte = c}
                              
moveUp :: GameState-> Map Coord Case -> GameState
moveUp gs@(GameState tx ty sp px py _) c = if (collision gs ((px- (fromIntegral tx))`div`20) (((py- (fromIntegral ty))-4)`div`20))
                                          || (collision gs (((px+25)- (fromIntegral tx))`div`20) (((py- (fromIntegral ty))-4)`div`20))
                                          || (collision gs (((px+12)- (fromIntegral tx))`div`20) (((py- (fromIntegral ty))-4)`div`20))
                                          then gs else gs { transY = ty + sp , carte = c}

moveDown :: GameState -> Map Coord Case -> GameState
moveDown gs@(GameState tx ty sp px py _ ) c = if (collision gs ((px- (fromIntegral tx))`div`20) ((((py+45)- (fromIntegral ty))+4)`div`20))
                                              || (collision gs (((px+12)- (fromIntegral tx))`div`20) ((((py+22)- (fromIntegral ty))+4)`div`20))
                                              || (collision gs (((px+25)- (fromIntegral tx))`div`20) ((((py+45)- (fromIntegral ty))+4)`div`20))
                                              then gs else gs { transY = ty - sp , carte = c}


collisionTile :: GameState -> Map Coord Case -> Bool
collisionTile gs@(GameState tx ty sp px py _ ) c 
                                                | (collision gs (((px- (fromIntegral tx))-4)`div`20) ((py- (fromIntegral ty))`div`20)) == True = True
                                                |

collision :: GameState ->CInt -> CInt -> Bool
collision gs@(GameState _ _ _ _ _ c ) x y = (case (Map.lookup (Coord x y) c) of
                                                Just Mur -> True
                                                Just Entree -> False
                                                Just Sortie -> False
                                                Nothing -> False)

----Outil de debuguage-------------

--renvoie la position du joueur ainsi que la valeur de la case associé à cette position
collision2 :: GameState -> IO ()
collision2 gs@(GameState tx ty _ px py c ) = do
  let value= (Map.lookup (Coord ((px- (fromIntegral tx))`div`20) ((py- (fromIntegral ty))`div`20) ) c)
  print ("--------", (Coord ((px- (fromIntegral tx))`div`20) ((py- (fromIntegral ty))`div`20) ), value, "-----------")

----------------------------------


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


