
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
                           ,terrain::Terrain
                           }
  deriving (Show)



initGameState :: CInt -> CInt-> Terrain -> GameState
initGameState px py terrain = GameState 0 0 4 px py terrain

--refreshMap:: GameState -> Map Coord Case -> GameState
--refreshMap gs@(GameState tx ty sp px py _) c = gs {transX = tx, transY=ty, speed=sp ,carte=c}

---------------------Deplacement du personnage (Ici, le déplacement du personnage est en réalité une translation de l'environnement, soit un scrolling)-------------
moveLeft :: GameState -> GameState
moveLeft gs@(GameState tx ty sp px py _)= if (collisionTileLeft gs px py ) then gs else gs { transX = tx + sp}

moveRight :: GameState -> GameState
moveRight gs@(GameState tx ty sp px py _)= if (collisionTileRight gs px py ) then gs else gs { transX = tx - sp}
                              
moveUp :: GameState -> GameState
moveUp gs@(GameState tx ty sp px py _)= if (collisionTileUp gs px py ) then gs else gs { transY = ty + sp }

moveDown :: GameState -> GameState
moveDown gs@(GameState tx ty sp px py _ )= if (collisionTileDown gs px py ) then gs else gs { transY = ty - sp }

-------------------Detection de collision pour chaqu'un des bord du personnages (Haut, bas, gauche, droite)------------------------
collisionTileLeft :: GameState -> CInt -> CInt -> Bool
collisionTileLeft gs@(GameState tx ty sp px py (Terrain  ht lg c)) x y   | (collision gs (((px- (fromIntegral tx))-4)`div`20) ((py- (fromIntegral ty))`div`20)) == True = True
                                                                          | py<y+45 = (collisionTileLeft (gs {persoY= py+1}) x y)
                                                                          | otherwise= False

collisionTileRight :: GameState -> CInt -> CInt  -> Bool
collisionTileRight gs@(GameState tx ty sp px py (Terrain  ht lg c) ) x y   | (collision gs ((((px+25)- (fromIntegral tx))+4)`div`20) ((py- (fromIntegral ty))`div`20)) == True = True
                                                                           | py<y+45 = (collisionTileRight (gs {persoY= py+1}) x y)
                                                                           | otherwise= False

collisionTileUp :: GameState -> CInt -> CInt  -> Bool
collisionTileUp gs@(GameState tx ty sp px py (Terrain ht lg c) ) x y   | (collision gs ((px- (fromIntegral tx))`div`20) (((py- (fromIntegral ty))-4)`div`20)) == True = True
                                                                       | px<x+25 = (collisionTileUp (gs {persoX= px+1}) x y)
                                                                       | otherwise= False

collisionTileDown :: GameState -> CInt -> CInt -> Bool
collisionTileDown gs@(GameState tx ty sp px py (Terrain  ht lg c) ) x y   | (collision gs ((px- (fromIntegral tx))`div`20) ((((py+45)- (fromIntegral ty))+4)`div`20)) == True = True
                                                                          | px<x+25 = (collisionTileDown (gs {persoX= px+1}) x y)
                                                                          | otherwise= False

------------------------------------------Detection de collision-------------------------------------
collision :: GameState ->CInt -> CInt -> Bool
collision gs@(GameState _ _ _ _ _ (Terrain  ht lg c) ) x y = (case (Map.lookup (Coord x y) c) of
                                                Just Mur -> True
                                                Just Coffre -> True
                                                Just Entree -> False
                                                Just Sortie -> False
                                                Nothing -> False)

----------------------Outil de debuguage--------------------------------------------------------

--renvoie la position du joueur ainsi que la valeur de la case associé à cette position
collision2 :: GameState -> IO ()
collision2 gs@(GameState tx ty _ px py (Terrain  ht lg c) ) = do
  let value= (Map.lookup (Coord ((px- (fromIntegral tx))`div`20) ((py- (fromIntegral ty))`div`20) ) c)
  print ("--------", (Coord ((px- (fromIntegral tx))`div`20) ((py- (fromIntegral ty))`div`20) ), value, "-----------")

------------------------------------------------------------------------------------------------

----------Mise a jour de l'était du jeu lors d'un déplacement------------------------
gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeZ kbd) && (K.keypressed KeycodeQ kbd) = gstate
                                    | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeS kbd) && (K.keypressed KeycodeQ kbd) = gstate
                                    | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeZ kbd) = (moveUp (moveRight gstate ) )
                                    | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeS kbd) = (moveDown (moveRight gstate ) )
                                    | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeZ kbd) = (moveUp (moveLeft gstate ) )
                                    | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeS kbd) = (moveDown (moveLeft gstate ) )
                                    | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeD kbd) = gstate
                                    | (K.keypressed KeycodeZ kbd) && (K.keypressed KeycodeS kbd) = gstate
                                    | (K.keypressed KeycodeD kbd) = (moveRight gstate )
                                    | (K.keypressed KeycodeQ kbd) = (moveLeft gstate )
                                    | (K.keypressed KeycodeZ kbd) = (moveUp gstate )
                                    | (K.keypressed KeycodeS kbd) = (moveDown gstate )
                                    | otherwise = gstate


