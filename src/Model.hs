
module Model where

import SDL

import Data.Map
import qualified Data.Map.Strict as Map

import Carte

import Foreign.C.Types (CInt (..) )

import Keyboard (Keyboard)
import qualified Keyboard as K

import System.Random
import Control.Concurrent (threadDelay)

--Transx : Correspond au déplacement sur l'axe des x du personnage (En réalité, c'est l'environnement qui se "deplace")
--Transy: Pareil que Transx mais sur l'axe des y
--PersoX: L'axe x où se situe le personnage
--PersoY: L'axe des y où se situe le personnage
data Translation = Translation { transX :: CInt
                                ,transY :: CInt
                                } 
                                deriving (Show)

data Perso = Perso { persoX :: CInt
                     ,persoY :: CInt
                     ,direction :: DirectionPerso
                    }
                    deriving (Show)

data GameState = GameState { translate :: Translation
                            ,tour :: Int
                            ,speed ::CInt
                            ,perso :: Perso 
                            ,terrain :: Terrain
                           }
  deriving (Show)

data DirectionPerso = North | West | South | East deriving (Eq,Show)

initGameState :: Translation -> Perso ->  Terrain -> GameState
initGameState translation perso terrain = GameState translation 0 4 perso terrain

--refreshMap:: GameState -> Map Coord Case -> GameState
--refreshMap gs@(GameState tx ty sp px py _) c = gs {transX = tx, transY=ty, speed=sp ,carte=c}

---------------------Deplacement du personnage (Ici, le déplacement du personnage est en réalité une translation de l'environnement, soit un scrolling)-------------
moveLeft :: GameState -> GameState
moveLeft gs@(GameState (Translation tx ty) _ sp (Perso px py _) _)= if (collisionTileLeft gs px py ) 
                                                                      then gs {perso = (Perso px py West)} 
                                                                        else gs {translate = (Translation (tx+sp) ty), perso = (Perso px py West)}

moveRight :: GameState -> GameState
moveRight gs@(GameState (Translation tx ty) _ sp (Perso px py _) _)= if (collisionTileRight gs px py ) 
                                                                    then gs {perso = (Perso px py East)}
                                                                      else gs {translate = (Translation (tx-sp) ty), perso = (Perso px py East)}
                              
moveUp :: GameState -> GameState
moveUp gs@(GameState (Translation tx ty) _ sp (Perso px py _) _)= if (collisionTileUp gs px py) 
                                                                  then gs {perso = (Perso px py North)}
                                                                    else gs {translate = (Translation tx (ty+sp)), perso = (Perso px py North)}

moveDown :: GameState -> GameState
moveDown gs@(GameState (Translation tx ty) _ sp (Perso px py _) _)= if (collisionTileDown gs px py)
                                                                    then gs {perso = (Perso px py South)}
                                                                      else gs {translate = (Translation tx (ty-sp)), perso = (Perso px py South)}

-------------------Detection de collision pour chaqu'un des bord du personnages (Haut, bas, gauche, droite)------------------------
collisionTileLeft :: GameState -> CInt -> CInt -> Bool
collisionTileLeft gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y  | (collision gs (coordonneesPx (fromIntegral tx) px (-4)) (coordonneesPy (fromIntegral ty) py 0)) == True = True --
                                                                                                | py<y+8 = (collisionTileLeft (gs {perso = (Perso px (py+1) d)}) x y)
                                                                                                | otherwise= False

collisionTileRight :: GameState -> CInt -> CInt  -> Bool
collisionTileRight gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y | (collision gs (coordonneesPx (fromIntegral tx) px 29) (coordonneesPy (fromIntegral ty) py 0)) == True = True
                                                                                                | py<y+8 = (collisionTileRight (gs {perso = (Perso px (py+1) d)}) x y)
                                                                                                | otherwise= False
-- Le 17 et 8 modifier pour collisiontileup collisiontiledown
collisionTileUp :: GameState -> CInt -> CInt  -> Bool
collisionTileUp gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y  | (collision gs (coordonneesPx (fromIntegral tx) px  8) (coordonneesPy (fromIntegral ty) py (-4))) == True = True
                                                                                              | px<x+17 = (collisionTileUp (gs {perso = (Perso (px+1) py d)}) x y)
                                                                                              | otherwise= False

collisionTileDown :: GameState -> CInt -> CInt -> Bool
collisionTileDown gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y | (collision gs (coordonneesPx (fromIntegral tx) px 8) (coordonneesPy (fromIntegral ty) py 20 )) == True = True
                                                                                               | px<x+17 = (collisionTileDown (gs {perso = (Perso (px+1) py d)}) x y)
                                                                                               | otherwise= False

------------------------------------------Detection de collision-------------------------------------
collision :: GameState ->CInt -> CInt -> Bool
collision gs@(GameState _ _ _ _ (Terrain  ht lg c)) x y = (case (Map.lookup (Coord x y) c) of
                                                Just Mur -> True
                                                Just (Coffre Ouvert) -> True
                                                Just (Coffre Ferme) -> True
                                                Just (Porte NS Ferme) -> True
                                                Just (Porte NS Ouvert) -> False
                                                Just (Porte EO Ferme) -> True
                                                Just (Porte EO Ouvert) -> False
                                                Just Entree -> False
                                                Just Sortie -> False
                                                Just Zombie -> True
                                                Nothing -> False)

objectOnPosition :: GameState -> CInt -> CInt-> String
objectOnPosition gs@(GameState _ _ _ _ (Terrain  ht lg c)) x y = (case (Map.lookup (Coord x y) c) of
                                                Just Mur -> "Mur"
                                                Just (Coffre Ouvert) -> "Coffre Ouvert"
                                                Just (Coffre Ferme) -> "Coffre Ferme"
                                                Just (Porte NS Ferme) -> "Porte NS"
                                                Just (Porte EO Ferme) -> "Porte EO"
                                                Just (Porte EO Ouvert) -> "Porte EO"
                                                Just (Porte NS Ouvert) -> "Porte NS"
                                                Just Entree -> "Entree"
                                                Just Sortie -> "Sortie"
                                                Just Zombie -> "Zombie"
                                                Nothing -> "Nothing")
--Pourquoi je fais tous ces calcules pour porte ferme et ouverte ?
{-
Pour une meilleuir précision je préfère tester tous les pixels pour savoir si la case adjacente au personnage est une porte. Dans le meilleur cas, 
le premier pixel trouve la porte comme étant une case adjacente au personnage (O(1)). Dans le pire cas, la case n'est pas trouver, on doit alors 
tester tous les pixels de côté du personnage. On a donc une complexité dans le pire cas en o(20)-> On test 20 pixels (Du la taille du perso,
jusqu'aux pieds)
-}


-- |Fonction d'action des portes

--Utiliser les directions pour améliorer le code des portes ?
--Complexité trop élevée pour "isitadDoor"..A modifier plus tard
isitaDoor:: GameState -> CInt -> CInt -> (CInt, CInt)
isitaDoor gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y | (isitaDoorLeft gs x y ) /= ((-1),(-1)) = (isitaDoorLeft gs x y )
                                                                                       | (isitaDoorRight gs x y ) /= ((-1),(-1)) = (isitaDoorRight gs x y)
                                                                                       | (isitaDoorUp gs x y ) /= ((-1),(-1)) = (isitaDoorUp gs x y)
                                                                                       | otherwise = (isitaDoorDown gs x y)

--Portes Ouest-Est
isitaDoorRight:: GameState -> CInt -> CInt -> (CInt, CInt)
isitaDoorRight gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y | (objectOnPosition gs ((coordonneesPx (fromIntegral tx) px 29)) (coordonneesPy (fromIntegral ty) py 0))=="Porte EO" = ((coordonneesPx (fromIntegral tx) px 29),(coordonneesPy (fromIntegral ty) py 0))
                                                                                            | py<y+8 = (isitaDoorRight (gs {perso = (Perso px (py+1) d)}) x y)
                                                                                            | otherwise = ((-1),(-1)) --Pas de porte

isitaDoorLeft:: GameState -> CInt -> CInt -> (CInt, CInt)
isitaDoorLeft gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y | (objectOnPosition gs (coordonneesPx (fromIntegral tx) px (-4)) (coordonneesPy (fromIntegral ty) py 0)) == "Porte EO" = ((coordonneesPx (fromIntegral tx) px (-4)),(coordonneesPy (fromIntegral ty) py 0))
                                                                                           | py<y+8 = (isitaDoorLeft (gs {perso = (Perso px (py+1) d)}) x y)
                                                                                           | otherwise = ((-1),(-1)) --Pas de porte
--Portes Nord-Sud
isitaDoorUp :: GameState -> CInt -> CInt  -> (CInt, CInt)
isitaDoorUp gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y  | (objectOnPosition gs (coordonneesPx (fromIntegral tx) px  0) (coordonneesPy (fromIntegral ty) py (-4)) ) == "Porte NS" = ((coordonneesPx (fromIntegral tx) px  0),(coordonneesPy (fromIntegral ty) py (-4)))
                                                                                          | px<x+25 = (isitaDoorUp (gs {perso = (Perso (px+1) py d)}) x y)
                                                                                          | otherwise = ((-1),(-1)) --Pas de porte

isitaDoorDown :: GameState -> CInt -> CInt -> (CInt, CInt)
isitaDoorDown gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y | (objectOnPosition gs (coordonneesPx (fromIntegral tx) px 0) (coordonneesPy (fromIntegral ty) py 24 ) ) == "Porte NS" = ((coordonneesPx (fromIntegral tx) px 0),(coordonneesPy (fromIntegral ty) py 24 ))
                                                                                           | px<x+25 = (isitaDoorDown (gs {perso = (Perso (px+1) py d)}) x y)
                                                                                           | otherwise = ((-1),(-1)) --Pas de porte

changeValueMap::GameState -> Coord -> Case -> GameState
changeValueMap gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) coord unecase = let newmap = (Map.insert coord unecase c ) in gs {terrain =(Terrain ht lg newmap)}

openaDoor::GameState -> CInt -> CInt -> GameState
openaDoor gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) a b | ((Map.lookup (Coord a b) c))== (Just (Porte NS Ferme) ) =  let f = (Porte NS Ouvert)  in (changeValueMap gs (Coord a b) f )
                                                                                       | ((Map.lookup (Coord a b) c))== (Just (Porte EO Ferme) ) = let f = (Porte EO Ouvert)  in (changeValueMap gs (Coord a b) f )
                                                                                       | ((Map.lookup (Coord a b) c))== (Just (Porte NS Ouvert) ) =  let f = (Porte NS Ferme)  in (changeValueMap gs (Coord a b) f )
                                                                                       | ((Map.lookup (Coord a b) c))== (Just (Porte EO Ferme) ) = let f = (Porte EO Ouvert)  in (changeValueMap gs (Coord a b) f )
                                                                                       | ((Map.lookup (Coord a b) c))== (Just (Porte EO Ouvert) ) = let f = (Porte EO Ferme)  in (changeValueMap gs (Coord a b) f )
                                                                                       | otherwise = gs
                                                            

testDoor::GameState -> GameState
testDoor gs@(GameState (Translation tx ty) _ sp (Perso px py _) (Terrain  ht lg c)) = let (a,b) =(isitaDoor gs px py) in 
                                                                  if (a,b) /= ((-1),(-1)) 
                                                                    then (openaDoor gs a b)
                                                                      else gs




-- |Fonction d'action des coffres

testChest :: GameState -> GameState
testChest gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) = let (a,b) =(isitChest gs px py) in 
                                                                  if (a,b) /= ((-1),(-1)) 
                                                                    then (openChest gs a b) 
                                                                      else gs


isitChest :: GameState -> CInt -> CInt -> (CInt, CInt)
isitChest gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y  | (isitChestLeft gs x y ) /= ((-1),(-1)) = (isitChestLeft gs x y )
                                                                                        | (isitChestRight gs x y ) /= ((-1),(-1)) = (isitChestRight gs x y)
                                                                                        | (isitChestUp gs x y ) /= ((-1),(-1)) = (isitChestUp gs x y)
                                                                                        | otherwise = (isitChestDown gs x y)


isitChestRight :: GameState -> CInt -> CInt -> (CInt, CInt)
isitChestRight gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y | (objectOnPosition gs ((coordonneesPx (fromIntegral tx) px 29)) (coordonneesPy (fromIntegral ty) py 0))=="Coffre Ferme" = ((coordonneesPx (fromIntegral tx) px 29),(coordonneesPy (fromIntegral ty) py 0))
                                                                                            | py<y+8 = (isitChestRight (gs {perso = (Perso px (py+1) d)}) x y)
                                                                                            | otherwise = ((-1),(-1)) --Pas de coffre

isitChestLeft :: GameState -> CInt -> CInt -> (CInt, CInt)
isitChestLeft gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y | (objectOnPosition gs (coordonneesPx (fromIntegral tx) px (-4)) (coordonneesPy (fromIntegral ty) py 0)) == "Coffre Ferme" = ((coordonneesPx (fromIntegral tx) px (-4)),(coordonneesPy (fromIntegral ty) py 0))
                                                                                           | py<y+8 = (isitChestLeft (gs {perso = (Perso px (py+1) d)}) x y)
                                                                                           | otherwise = ((-1),(-1)) --Pas de coffre

              
isitChestUp :: GameState -> CInt -> CInt -> (CInt, CInt)
isitChestUp gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y  | (objectOnPosition gs ((coordonneesPx (fromIntegral tx) px 0)) (coordonneesPy (fromIntegral ty) py (-4)))=="Coffre Ferme" = ((coordonneesPx (fromIntegral tx) px 0),(coordonneesPy (fromIntegral ty) py (-4)))
                                                                                          | px<x+25 = (isitChestUp (gs {perso = (Perso (px+1) py d)}) x y)
                                                                                          | otherwise = ((-1),(-1)) --Pas de coffre

isitChestDown :: GameState -> CInt -> CInt -> (CInt, CInt)
isitChestDown gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) x y  | (objectOnPosition gs (coordonneesPx (fromIntegral tx) px 0) (coordonneesPy (fromIntegral ty) py 24)) == "Coffre Ferme" = ((coordonneesPx (fromIntegral tx) px 0),(coordonneesPy (fromIntegral ty) py 24))
                                                                                            | px<x+25 = (isitChestDown (gs {perso = (Perso (px+1) py d)}) x y)
                                                                                            | otherwise = ((-1),(-1)) --Pas de coffre

openChest::GameState -> CInt -> CInt -> GameState
openChest gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) a b  | ((Map.lookup (Coord a b) c))== (Just (Coffre Ferme) ) =  let f = (Coffre Ouvert)  in (changeValueMap gs (Coord a b) f )
                                                                                        -- | ((Map.lookup (Coord a b) c))== (Just (Coffre Ouvert) ) =  let f = (Coffre Ferme)  in (changeValueMap gs (Coord a b) f )
                                                                                        | otherwise = gs

-- |IA

action:: GameState -> IO (GameState)
action gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c))= do
  let entites = getCoordonneesObjectMap c (Just Zombie)
  (moveAll gs entites)

moveAll::GameState -> [Coord] -> IO (GameState)
moveAll gs [] = return gs
moveAll gs (x:xs) = do
  newgs <- move gs x
  moveAll newgs xs

move:: GameState -> Coord -> IO (GameState)
move gs@(GameState (Translation tx ty) _ sp (Perso px py d) (Terrain  ht lg c)) (Coord x y)= do
  alea <-randomRIO (1,4) :: IO Int
  case alea of
    1 -> if ((Map.lookup (Coord x (y-1)) c)) == Nothing then  return gs { terrain = (Terrain ht lg (updateKeyMap (Coord x y) (Coord x (y-1)) c))} else return gs
    2 -> if ((Map.lookup (Coord x (y+1)) c)) == Nothing then return gs { terrain = (Terrain ht lg (updateKeyMap (Coord x y) (Coord x (y+1)) c))} else return gs
    3 -> if ((Map.lookup (Coord (x+1) y) c)) == Nothing then return gs { terrain = (Terrain ht lg (updateKeyMap (Coord x y) (Coord (x+1) y) c))} else return gs
    4 -> if ((Map.lookup (Coord (x-1) y) c)) == Nothing then return gs { terrain = (Terrain ht lg (updateKeyMap (Coord x y) (Coord (x-1) y) c))} else return gs


-- |Utils
updateKeyMap :: Coord -> Coord -> Map Coord Case -> Map Coord Case
updateKeyMap k0 k1 myMap = case Map.lookup k0 myMap of
   Nothing -> myMap
   Just e  -> Map.insert k1 e (Map.delete k0 myMap)

-- |Outils de debuguage

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
collision2 gs@(GameState (Translation tx ty) _ sp (Perso px py _) (Terrain  ht lg c)) = do
  let door= isitaDoorRight gs (coordonneesPx (fromIntegral tx) px 29) (coordonneesPy (fromIntegral ty) py 0)
  let touttile=(collisionTileRight gs px py )
  let value= (Map.lookup (Coord (coordonneesPx (fromIntegral tx) px 29) (coordonneesPy (fromIntegral ty) py 0) ) c)
  print ()
  --print ( "-------", door)
  --print ("--------", (Coord (coordonneesPx (fromIntegral tx) px 29) (coordonneesPy (fromIntegral ty) py 0) ), value, touttile,"-----------")

------------------------------------------------------------------------------------------------



-- |Mise a jour de l'était du jeu lors d'un déplacement

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeZ kbd) && (K.keypressed KeycodeQ kbd) = gstate
                                    | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeS kbd) && (K.keypressed KeycodeQ kbd) = gstate
                                    | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeZ kbd) = (moveUp (moveRight gstate))
                                    | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeS kbd) = (moveDown (moveRight gstate))
                                    | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeZ kbd) = (moveUp (moveLeft gstate))
                                    | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeS kbd) = (moveDown (moveLeft gstate))
                                    | (K.keypressed KeycodeQ kbd) && (K.keypressed KeycodeD kbd) = gstate
                                    | (K.keypressed KeycodeZ kbd) && (K.keypressed KeycodeS kbd) = gstate
                                    | (K.keypressed KeycodeE kbd) = (testChest (testDoor gstate)) 
                                    | (K.keypressed KeycodeD kbd) = (moveRight gstate)
                                    | (K.keypressed KeycodeQ kbd) = (moveLeft gstate)
                                    | (K.keypressed KeycodeZ kbd) = (moveUp gstate)
                                    | (K.keypressed KeycodeS kbd) = (moveDown gstate)
                                    | otherwise = gstate
