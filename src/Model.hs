
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
moveUp gs@(GameState tx ty sp px py _)= if (collisionTileUp gs px py) then gs else gs { transY = ty + sp }

moveDown :: GameState -> GameState
moveDown gs@(GameState tx ty sp px py _ )= if (collisionTileDown gs px py) then gs else gs { transY = ty - sp }

-------------------Detection de collision pour chaqu'un des bord du personnages (Haut, bas, gauche, droite)------------------------
collisionTileLeft :: GameState -> CInt -> CInt -> Bool
collisionTileLeft gs@(GameState tx ty sp px py (Terrain  ht lg c)) x y    | (collision gs (coordonneesPx (fromIntegral tx) px (-4)) (coordonneesPy (fromIntegral ty) py 0)) == True = True --
                                                                          | py<y+8 = (collisionTileLeft (gs {persoY= py+1}) x y)
                                                                          | otherwise= False

collisionTileRight :: GameState -> CInt -> CInt  -> Bool
collisionTileRight gs@(GameState tx ty sp px py (Terrain  ht lg c) ) x y   | (collision gs (coordonneesPx (fromIntegral tx) px 29) (coordonneesPy (fromIntegral ty) py 0)) == True = True
                                                                           | py<y+8 = (collisionTileRight (gs {persoY= py+1}) x y)
                                                                           | otherwise= False

collisionTileUp :: GameState -> CInt -> CInt  -> Bool
collisionTileUp gs@(GameState tx ty sp px py (Terrain ht lg c)) x y    | (collision gs (coordonneesPx (fromIntegral tx) px  0) (coordonneesPy (fromIntegral ty) py (-4))) == True = True
                                                                       | px<x+25 = (collisionTileUp (gs {persoX= px+1}) x y)
                                                                       | otherwise= False

collisionTileDown :: GameState -> CInt -> CInt -> Bool
collisionTileDown gs@(GameState tx ty sp px py (Terrain  ht lg c) ) x y   | (collision gs (coordonneesPx (fromIntegral tx) px 0) (coordonneesPy (fromIntegral ty) py 24 )) == True = True
                                                                          | px<x+25 = (collisionTileDown (gs {persoX= px+1}) x y)
                                                                          | otherwise= False

------------------------------------------Detection de collision-------------------------------------
collision :: GameState ->CInt -> CInt -> Bool
collision gs@(GameState _ _ _ _ _ (Terrain  ht lg c)) x y = (case (Map.lookup (Coord x y) c) of
                                                Just Mur -> True
                                                Just Coffre -> True
                                                Just (Porte EO Ferme) -> True
                                                Just (Porte EO Ferme) -> True
                                                Just Entree -> False
                                                Just Sortie -> False
                                                Nothing -> False)

----------------------Outil de debuguage--------------------------------------------------------
{------Concernant les valeurs fixes dans les collisions: 
-> ligne 56 : Le -4 permet d'éviter de se retrouver dans un mur. Les blocs sont de 20 pixel mais le perso se déplace de 4pixel, on peut donc observer un ecart de 4 pixel qui fait rentrer une petite partie dans le mur.
  Consequences : On ne peux plus longer un mur du haut vers le bas
->ligne 61: Le 29 concerne un problème de sprite. Les sprites son positionnés avec des coordonées placé en haut à gauche (0,0). La collision sera effective sur la tranche droite du bloc. On comble se décalage tel que 29 = 25 (la largeur du personnage) + 4 (Le déplacement de 4 pixel)
  Consequences:  On ne peux plus longer un mur du haut vers le bas
->ligne 66: pareil que la ligne 56.
  Consequences : On ne peux plus longer un mur de l'est vers l'ouest
->ligne 71 : 24 = 20 (bloc du bac) + 4 (déplcement du personnage en pixel)

-}


--Coordonnees stricte personnage axe X
coordonneesPx:: CInt -> CInt -> CInt -> CInt
coordonneesPx tx px x = (((px+x)-(fromIntegral tx))`div`20)

--Coordonnees stricte personnage axe Y
coordonneesPy:: CInt -> CInt -> CInt -> CInt
coordonneesPy ty py y= (((py+25+y)-(fromIntegral ty))`div`20)

--renvoie la position du joueur ainsi que la valeur de la case associé à cette position
collision2 :: GameState -> IO ()
collision2 gs@(GameState tx ty _ px py (Terrain  ht lg c) ) = do
  let touttile=(collisionTileRight gs px py )
  let value= (Map.lookup (Coord (coordonneesPx (fromIntegral tx) px 0) (coordonneesPy (fromIntegral ty) px 0) ) c)
  print ("--------", (Coord (coordonneesPx (fromIntegral tx) px 0) (coordonneesPy (fromIntegral ty) px 0) ), value, touttile,"-----------")

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


