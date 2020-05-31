
module Model where

import SDL

import Data.Map
import qualified Data.Map.Strict as Map

import Carte
import qualified Carte as C

import Foreign.C.Types (CInt (..) )

import Keyboard (Keyboard)
import qualified Keyboard as K

import System.Random
import Control.Concurrent (threadDelay)

--Transx : Correspond au déplacement sur l'axe des x du personnage (En réalité, c'est l'environnement qui se "deplace")
--Transy : Pareil que Transx mais sur l'axe des y
--PersoX : L'axe x où se situe le personnage
--PersoY : L'axe des y où se situe le personnage
data Translation = Translation { transX :: CInt
                                ,transY :: CInt
                                } 
                                deriving (Show,Eq)

data Perso = Perso { persoX :: CInt
                    ,persoY :: CInt
                    ,direction :: DirectionPerso
                    ,vie :: CInt
                    ,inventaire :: Map Item CInt
                    }
                    deriving (Show,Eq)

data Item = Potion
            | Masque
            deriving (Show,Eq,Ord)

data EtatJeu = Gagner 
              | Perdu
              | Encours
              deriving (Show,Eq)

data GameState = GameState { translate :: Translation
                            ,tour :: Int
                            ,speed ::CInt
                            ,perso :: Perso 
                            ,terrain :: Terrain
                            ,etatjeu :: EtatJeu
                           }
  deriving (Show, Eq) 

data DirectionPerso = North | West | South | East deriving (Eq,Show)

initGameState :: Translation -> Perso ->  Terrain -> GameState
initGameState translation perso terrain = GameState translation 0 4 perso terrain Encours

--refreshMap:: GameState -> Map Coord Case -> GameState
--refreshMap gs@(GameState tx ty sp px py _) c = gs {transX = tx, transY=ty, speed=sp ,carte=c}

------Deplacement du personnage (Ici, le déplacement du personnage est en réalité une translation de l'environnement, soit un scrolling)-------------
moveLeft :: GameState -> GameState
moveLeft gs@(GameState (Translation tx ty) _ sp (Perso px py _ v inv) _ _)= if (collisionTileLeft gs px py ) 
                                                        then gs {perso = (Perso px py West v inv)} 
                                                        else gs {translate = (Translation (tx+sp) ty), perso = (Perso px py West v inv)}

moveRight :: GameState -> GameState
moveRight gs@(GameState (Translation tx ty) _ sp (Perso px py _ v inv) _ _)= if (collisionTileRight gs px py ) 
                                                        then gs {perso = (Perso px py East v inv)}
                                                        else gs {translate = (Translation (tx-sp) ty), perso = (Perso px py East v inv)}
                              
moveUp :: GameState -> GameState
moveUp gs@(GameState (Translation tx ty) _ sp (Perso px py _ v inv) _ _)= if (collisionTileUp gs px py) 
                                                        then gs {perso = (Perso px py North v inv)}
                                                        else gs {translate = (Translation tx (ty+sp)), perso = (Perso px py North v inv)}

moveDown :: GameState -> GameState
moveDown gs@(GameState (Translation tx ty) _ sp (Perso px py _ v inv) _ _)= if (collisionTileDown gs px py)
                                                        then gs {perso = (Perso px py South v inv)}
                                                        else gs {translate = (Translation tx (ty-sp)), perso = (Perso px py South v inv)}

-------------------Detection de collision pour chaqu'un des bord du personnages (Haut, bas, gauche, droite)------------------------
collisionTileLeft :: GameState -> CInt -> CInt -> Bool
collisionTileLeft gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) x y  
  | (collision c (coordonneesPx (fromIntegral tx) px (-4)) (coordonneesPy (fromIntegral ty) py 0)) == True = True --
  | py<y+8 = (collisionTileLeft (gs {perso = (Perso px (py+1) d v inv)}) x y)
  | otherwise= False

collisionTileRight :: GameState -> CInt -> CInt  -> Bool
collisionTileRight gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) x y 
  | (collision c (coordonneesPx (fromIntegral tx) px 27) (coordonneesPy (fromIntegral ty) py 0)) == True = True
  | py<y+8 = (collisionTileRight (gs {perso = (Perso px (py+1) d v inv)}) x y)
  | otherwise= False

collisionTileUp :: GameState -> CInt -> CInt  -> Bool
collisionTileUp gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) x y  
  | (collision c (coordonneesPx (fromIntegral tx) px  8) (coordonneesPy (fromIntegral ty) py (-4))) == True = True
  | px<x+15 = (collisionTileUp (gs {perso = (Perso (px+1) py d v inv)}) x y)
  | otherwise= False

collisionTileDown :: GameState -> CInt -> CInt -> Bool
collisionTileDown gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) x y 
  | (collision c (coordonneesPx (fromIntegral tx) px 8) (coordonneesPy (fromIntegral ty) py 20 )) == True = True
  | px<x+15 = (collisionTileDown (gs {perso = (Perso (px+1) py d v inv)}) x y)
  | otherwise= False

------------------------------------------Detection de collision-------------------------------------
-- -4 explications pour tileup : peso se déplace de 4 pixel

--Pourquoi je fais tous ces calcules pour porte ferme et ouverte ?
{-
Pour une meilleur précision je préfère tester tous les pixels pour savoir si la case adjacente au personnage est une porte. 
Dans le meilleur cas, le premier pixel trouve la porte comme étant une case adjacente au personnage (O(1)). 
Dans le pire cas, la case n'est pas trouver, on doit alors tester tous les pixels de côté du personnage. 
On a donc une complexité dans le pire cas en o(20)-> On test 20 pixels (Du la taille du perso, jusqu'aux pieds)
-}

-- | Fonction de detection d'un objet en particulier dont la collision doit être stricte (Cad : detection au bord près de l'objet)
isitanEntity :: GameState -> String -> CInt -> CInt -> (CInt, CInt)
isitanEntity gs entity x y | (isitanEntityLeft gs entity x y ) /= ((-1),(-1)) = (isitanEntityLeft gs entity x y )
                           | (isitanEntityRight gs entity x y ) /= ((-1),(-1)) = (isitanEntityRight gs entity x y)
                           | (isitanEntityUp gs entity x y ) /= ((-1),(-1)) = (isitanEntityUp gs entity x y)
                           | otherwise = (isitanEntityDown gs entity x y)

isitanEntityRight :: GameState -> String -> CInt -> CInt -> (CInt, CInt)
isitanEntityRight gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) entity x y 
  | (objectOnPosition c ((coordonneesPx (fromIntegral tx) px 29)) (coordonneesPy (fromIntegral ty) py 0))== entity = 
      ((coordonneesPx (fromIntegral tx) px 29),(coordonneesPy (fromIntegral ty) py 0))
  | py<y+8 = (isitanEntityRight (gs {perso = (Perso px (py+1) d v inv)}) entity x y)
  | otherwise = ((-1),(-1))

isitanEntityLeft :: GameState -> String -> CInt -> CInt -> (CInt, CInt)
isitanEntityLeft gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) entity x y 
  | (objectOnPosition c (coordonneesPx (fromIntegral tx) px (-4)) (coordonneesPy (fromIntegral ty) py 0)) == entity = 
      ((coordonneesPx (fromIntegral tx) px (-4)),(coordonneesPy (fromIntegral ty) py 0))                        
  | py<y+8 = (isitanEntityLeft (gs {perso = (Perso px (py+1) d v inv)}) entity x y)                                                                                                          
  | otherwise = ((-1),(-1))

isitanEntityUp :: GameState -> String -> CInt -> CInt  -> (CInt, CInt)
isitanEntityUp gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) entity x y  
  | (objectOnPosition c (coordonneesPx (fromIntegral tx) px  0) (coordonneesPy (fromIntegral ty) py (-8)) ) == entity = 
      ((coordonneesPx (fromIntegral tx) px  0),(coordonneesPy (fromIntegral ty) py (-4)))
  | px<x+25 = (isitanEntityUp (gs {perso = (Perso (px+1) py d v inv)}) entity x y)
  | otherwise = ((-1),(-1))

isitanEntityDown :: GameState -> String -> CInt -> CInt -> (CInt, CInt)
isitanEntityDown gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) entity x y 
  | (objectOnPosition c (coordonneesPx (fromIntegral tx) px 0) (coordonneesPy (fromIntegral ty) py 20 ) ) == entity = 
      ((coordonneesPx (fromIntegral tx) px 0),(coordonneesPy (fromIntegral ty) py 24 ))
  | px<x+25 = (isitanEntityDown (gs {perso = (Perso (px+1) py d v inv)}) entity x y)
  | otherwise = ((-1),(-1))

openEntity :: GameState -> String -> CInt -> CInt -> GameState
openEntity gs@(GameState _ _ _ _ (Terrain  ht lg c) _) entity a b 
  | objectOnPosition c a b == entity =  let f = C.getCaseFromString entity in 
                                          gs {terrain =(Terrain ht lg (updateValueMap c (Coord a b) f ))}
  | otherwise = gs

-- | Fonction de detection d'un objet en particulier dont la collision est une hitbox réduite 
-- (cad: detection lorsque l'on s'approche du centre)
isitanEntityFlex :: GameState -> String -> CInt -> CInt -> (CInt, CInt)
isitanEntityFlex gs entity x y | (isitanEntityLeftFlex gs entity x y ) /= ((-1),(-1)) = (isitanEntityLeftFlex gs entity x y )
                               | (isitanEntityRightFlex gs entity x y ) /= ((-1),(-1)) = (isitanEntityRightFlex gs entity x y)
                               | (isitanEntityUpFlex gs entity x y ) /= ((-1),(-1)) = (isitanEntityUpFlex gs entity x y)
                               | otherwise = (isitanEntityDownFlex gs entity x y)

isitanEntityRightFlex :: GameState -> String -> CInt -> CInt -> (CInt, CInt)
isitanEntityRightFlex gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) entity x y 
  | (objectOnPosition c ((coordonneesPx (fromIntegral tx) px 12)) (coordonneesPy (fromIntegral ty) py 0))== entity = 
      ((coordonneesPx (fromIntegral tx) px 29),(coordonneesPy (fromIntegral ty) py 0))
  | py<y+17 = (isitanEntityRightFlex (gs {perso = (Perso px (py+1) d v inv)}) entity x y)
  | otherwise = ((-1),(-1))

isitanEntityLeftFlex :: GameState -> String -> CInt -> CInt -> (CInt, CInt)
isitanEntityLeftFlex gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) entity x y 
  | (objectOnPosition c (coordonneesPx (fromIntegral tx) px 12) (coordonneesPy (fromIntegral ty) py 0)) == entity = 
      ((coordonneesPx (fromIntegral tx) px (-4)),(coordonneesPy (fromIntegral ty) py 0))
  | py<y+17 = (isitanEntityLeftFlex (gs {perso = (Perso px (py+1) d v inv)}) entity x y)
  | otherwise = ((-1),(-1))

isitanEntityUpFlex :: GameState -> String -> CInt -> CInt  -> (CInt, CInt)
isitanEntityUpFlex gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) entity x y 
  | (objectOnPosition c (coordonneesPx (fromIntegral tx) px  8) (coordonneesPy (fromIntegral ty) py 0) ) == entity = 
      ((coordonneesPx (fromIntegral tx) px  0),(coordonneesPy (fromIntegral ty) py (-4)))
  | px<x+15 = (isitanEntityUpFlex (gs {perso = (Perso (px+1) py d v inv)}) entity x y)
  | otherwise = ((-1),(-1))

isitanEntityDownFlex :: GameState -> String -> CInt -> CInt -> (CInt, CInt)
isitanEntityDownFlex gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) entity x y 
  | (objectOnPosition c (coordonneesPx (fromIntegral tx) px 8) (coordonneesPy (fromIntegral ty) py 16 ) ) == entity = 
      ((coordonneesPx (fromIntegral tx) px 0),(coordonneesPy (fromIntegral ty) py 24 ))
  | px<x+15= (isitanEntityDownFlex (gs {perso = (Perso (px+1) py d v inv)}) entity x y)
  | otherwise = ((-1),(-1))

-- |Fonction d'action des portes

--Utiliser les directions pour améliorer le code des portes ?
--Complexité trop élevée pour "isitadDoor"..A modifier plus tard
isitaDoor :: GameState -> CInt -> CInt -> (CInt, CInt)
isitaDoor gs x y | (isitaDoorLeft gs x y ) /= ((-1),(-1)) = (isitaDoorLeft gs x y )
                 | (isitaDoorRight gs x y ) /= ((-1),(-1)) = (isitaDoorRight gs x y)
                 | (isitaDoorUp gs x y ) /= ((-1),(-1)) = (isitaDoorUp gs x y)
                 | otherwise = (isitaDoorDown gs x y)

--True si une porte est adjacente sinon false
isitaDoor_post ::  GameState -> CInt -> CInt -> Bool
isitaDoor_post gs x y = isitaDoor gs x y /= ((-1),(-1))

--Portes Ouest-Est
isitaDoorRight :: GameState -> CInt -> CInt -> (CInt, CInt)
isitaDoorRight gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) x y 
  | (objectOnPosition c ((coordonneesPx (fromIntegral tx) px 29)) (coordonneesPy (fromIntegral ty) py (-8)))=="Porte EO" = 
      ((coordonneesPx (fromIntegral tx) px 29),(coordonneesPy (fromIntegral ty) py 0))
  | py<y+8 = (isitaDoorRight (gs {perso = (Perso px (py+1) d v inv)}) x y)
  | otherwise = ((-1),(-1)) --Pas de porte

isitaDoorLeft :: GameState -> CInt -> CInt -> (CInt, CInt)
isitaDoorLeft gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) x y 
  | (objectOnPosition c (coordonneesPx (fromIntegral tx) px (-4)) (coordonneesPy (fromIntegral ty) py 0)) == "Porte EO" = 
      ((coordonneesPx (fromIntegral tx) px (-4)),(coordonneesPy (fromIntegral ty) py 0))
  | py<y+8 = (isitaDoorLeft (gs {perso = (Perso px (py+1) d v inv)}) x y)
  | otherwise = ((-1),(-1)) --Pas de porte

--Portes Nord-Sud
isitaDoorUp :: GameState -> CInt -> CInt  -> (CInt, CInt)
isitaDoorUp gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) x y  
  | (objectOnPosition c (coordonneesPx (fromIntegral tx) px  0) (coordonneesPy (fromIntegral ty) py (-4)) ) == "Porte NS" = 
      ((coordonneesPx (fromIntegral tx) px  0),(coordonneesPy (fromIntegral ty) py (-4)))
  | px<x+25 = (isitaDoorUp (gs {perso = (Perso (px+1) py d v inv)}) x y)
  | otherwise = ((-1),(-1)) --Pas de porte

isitaDoorDown :: GameState -> CInt -> CInt -> (CInt, CInt)
isitaDoorDown gs@(GameState (Translation tx ty) _ _ (Perso px py d v inv) (Terrain  _ _ c) _) x y 
  | (objectOnPosition c (coordonneesPx (fromIntegral tx) px 0) (coordonneesPy (fromIntegral ty) py 24 ) ) == "Porte NS" = 
      ((coordonneesPx (fromIntegral tx) px 0),(coordonneesPy (fromIntegral ty) py 24 ))
  | px<x+25 = (isitaDoorDown (gs {perso = (Perso (px+1) py d v inv)}) x y)
  | otherwise = ((-1),(-1)) --Pas de porte



openaDoor :: GameState -> CInt -> CInt -> GameState
openaDoor gs@(GameState _ _ _ _ (Terrain  ht lg c) _) a b 
  | ((Map.lookup (Coord a b) c)) == (Just (Porte NS Ferme) ) = let f = (Porte NS Ouvert) in 
                                                                gs {terrain =(Terrain ht lg (C.updateValueMap c (Coord a b) f ))}
  | ((Map.lookup (Coord a b) c)) == (Just (Porte EO Ferme) ) = let f = (Porte EO Ouvert) in 
                                                                gs {terrain =(Terrain ht lg (C.updateValueMap c (Coord a b) f ))}
  | ((Map.lookup (Coord a b) c)) == (Just (Porte NS Ouvert)) = let f = (Porte NS Ferme)  in 
                                                                gs {terrain =(Terrain ht lg (C.updateValueMap c (Coord a b) f ))}
  | ((Map.lookup (Coord a b) c)) == (Just (Porte EO Ouvert)) = let f = (Porte EO Ferme)  in 
                                                                gs {terrain =(Terrain ht lg (C.updateValueMap c (Coord a b) f ))}
  | otherwise = gs

openaDoor_pre :: GameState -> CInt -> CInt -> Bool
openaDoor_pre gs@(GameState _ _ _ _ (Terrain  _ _ c) _) a b 
  | ((Map.lookup (Coord a b) c)) == (Just (Porte NS Ferme) ) = True
  | ((Map.lookup (Coord a b) c)) == (Just (Porte EO Ferme) ) = True
  | ((Map.lookup (Coord a b) c)) == (Just (Porte NS Ouvert)) = True
  | ((Map.lookup (Coord a b) c)) == (Just (Porte EO Ouvert)) = True
  | otherwise = False

testDoor :: GameState -> GameState
testDoor gs@(GameState _ _ _ (Perso px py _ _ _) (Terrain  _ _ c) _) = let (a,b) = (isitaDoor gs px py) in 
                                                                        if (a,b) /= ((-1),(-1)) 
                                                                          then (openaDoor gs a b)
                                                                          else gs
testDoor_pre :: GameState -> Bool
testDoor_pre gs@(GameState _ _ _ _ (Terrain  _ _ c) _) = testMap c "Porte Ferme"

-- |Fonction d'action des coffres
testChest :: GameState -> GameState
testChest gs@(GameState _ _ _ (Perso px py d _ _) (Terrain  _ _ c) _) = let (a,b) = (isitanEntity gs "Coffre Ferme" px py) in 
                                                                        if (a,b) /= ((-1),(-1)) 
                                                                          then (openChest gs "Coffre Ferme" a b) 
                                                                          else gs

testChest_pre :: GameState -> Bool
testChest_pre gs@(GameState _ _ _ _ (Terrain  _ _ c) _) = testMap c "Coffre Ferme"

openChest :: GameState -> String -> CInt -> CInt -> GameState
openChest gs@(GameState _ _ _ _ (Terrain  ht lg c) _) entity a b 
  | objectOnPosition c a b == entity =  let f = C.getCaseFromString entity in 
                                          looting (gs {terrain =(Terrain ht lg (updateValueMap c (Coord a b) f ))})
  | otherwise = gs

-- |Fonction d'action des tresor (identique aux coffres)
testTresor :: GameState -> GameState
testTresor gs@(GameState _ _ _ (Perso px py d _ _) (Terrain  _ _ c) _) = let (a,b) = (isitanEntity gs "Tresor Ferme" px py) in 
                                                                        if (a,b) /= ((-1),(-1)) 
                                                                          then (openTresor gs "Tresor Ferme" a b) 
                                                                          else gs

testTresor_pre :: GameState -> Bool
testTresor_pre gs@(GameState _ _ _ _ (Terrain  _ _ c) _) = testMap c "Tresor Ferme"

openTresor :: GameState -> String -> CInt -> CInt -> GameState
openTresor gs@(GameState _ _ _ _ (Terrain  ht lg c) _) entity a b 
  | objectOnPosition c a b == entity =  let f = C.getCaseFromString entity in 
                                          lootingTreasure (gs {terrain =(Terrain ht lg (updateValueMap c (Coord a b) f ))})
  | otherwise = gs

--Permet de récuperer un objet présent dans un coffre (Pour l'instant une potion seulement)
looting :: GameState -> GameState
looting gs@(GameState _ _ _ (Perso px py d v inv) _ _) = gs {perso = (Perso px py d v (adjust (+ 1) Potion inv))}

--Permet de récuperer le tresor permettant de sortir
lootingTreasure :: GameState -> GameState
lootingTreasure gs@(GameState _ _ _ (Perso px py d v inv) _ _) = gs {perso = (Perso px py d v (adjust (+ 1) Masque inv))}

-- |Fonctions d'inventaire

usePotion :: GameState -> GameState
usePotion gs@(GameState _ _ _ (Perso px py d v inv) _ _) = if inv ! Potion == 0 
                                                            then gs
                                                            else changePv (gs {perso = (Perso px py d v (adjust (+ (-1)) Potion inv)) }) 20

-- |Fonction de Sortie
testSortie :: GameState -> Bool
testSortie gs@(GameState _ _ _ (Perso px py _ _ inv) _ _) = 
  let (a,b) =(isitanEntityFlex gs "Sortie" px py) in (a,b) /= ((-1),(-1)) && (inv ! Masque) == 1

testSortie_pre :: GameState -> Bool
testSortie_pre gs@(GameState _ _ _ _ (Terrain  _ _ c) _) = testMap c "Sortie"

-- | Fonctions lié aux pieges

--detecte si le personnage est tombé dans un piege
tombeDansPiege:: GameState -> Bool
tombeDansPiege gs@(GameState _ _ _ (Perso px py _ _ _) _ _) =
  (isitanEntityFlex gs "Pique Ferme" px py) /= ((-1),(-1)) 
  || (isitanEntityFlex gs "Pique Ouvert" px py) /= ((-1),(-1))
  || (isitanEntityFlex gs "ClotureElectrique NS Ouvert" px py) /= ((-1),(-1))
  || (isitanEntityFlex gs "ClotureElectrique EO Ouvert" px py) /= ((-1),(-1))

testPiege :: GameState -> GameState
testPiege gs@(GameState _ _ _ (Perso px py _ _ _) _ _) 
  | (isitanEntityFlex gs "Pique Ferme" px py) /= ((-1),(-1)) = actionPiqueFerme gs
  | (isitanEntityFlex gs "Pique Ouvert" px py) /= ((-1),(-1)) = actionPiqueOuvert gs
  | (isitanEntityFlex gs "ClotureElectrique NS Ouvert" px py) /= ((-1),(-1)) = actionClotureElectrique gs "NS"
  | (isitanEntityFlex gs "ClotureElectrique EO Ouvert" px py) /= ((-1),(-1)) = actionClotureElectrique gs "EO"
  | otherwise = gs

actionPiqueFerme :: GameState -> GameState
actionPiqueFerme gs@(GameState _ _ _ (Perso px py _ _ _) _ _) =
  let (x,y) = (isitanEntityFlex gs "Pique Ferme" px py) in checkProjection $ openEntity (changePv gs (-10)) "Pique Ferme" x y

actionPiqueOuvert :: GameState -> GameState
actionPiqueOuvert gs@(GameState _ _ _ (Perso px py _ _ _) _ _) =
  let (x,y) = (isitanEntityFlex gs "Pique Ouvert" px py) in checkProjection $ changePv gs (-10)

--Fonction d'action d'une CLoture Electrique
actionClotureElectrique :: GameState-> String -> GameState
actionClotureElectrique gs@(GameState _ _ _ (Perso px py _ _ _) _ _) directionCloture
  | directionCloture=="NS" = let (x,y) = (isitanEntityFlex gs "ClotureElectrique NS Ouvert" px py) in checkProjection $ changePv gs (-20)
  | directionCloture== "EO" = let (x,y) = (isitanEntityFlex gs "ClotureElectrique EO Ouvert" px py) in checkProjection $ changePv gs (-20)


--Fonction de levier
testLevier :: GameState -> GameState
testLevier gs@(GameState _ _ _ (Perso px py _ _ _) _ _) = 
  let (a,b) =(isitanEntity gs "Levier Ferme" px py) 
    in if (a,b) /= ((-1),(-1)) 
        then (actionLevier gs (Coord a b))
          else gs


actionLevier:: GameState -> Coord -> GameState
actionLevier gs@(GameState _ _ _ _ (Terrain  _ _ carte) _) levierCoord=  
  let clotures = (getCoordonneesObjectMap carte (Just (ClotureElectrique NS Ouvert))) ++
                 (getCoordonneesObjectMap carte (Just (ClotureElectrique EO Ouvert))) in
    if (length clotures> 0) then auxActionLevier2 gs clotures levierCoord (Coord 0 0) (maxBound::CInt) "" else gs



auxActionLevier2:: GameState -> [Coord] -> Coord -> Coord-> CInt -> String -> GameState
auxActionLevier2 gs [] levierCoord coordObjet _ direction= openLevier gs direction levierCoord coordObjet
auxActionLevier2 gs@(GameState _ _ _ _ (Terrain  _ _ carte) _) (x:xs) levierCoord coord value direction 
  | C.distance levierCoord x < value = let (Coord a b) = x in 
      auxActionLevier2 gs xs levierCoord x (C.distance levierCoord x) (if (objectOnPosition carte a b)=="ClotureElectrique NS Ouvert" then "NS" else "EO")
  | otherwise = auxActionLevier2 gs xs levierCoord coord value direction


openLevier:: GameState -> String ->Coord -> Coord -> GameState
openLevier gs directionCloture (Coord lx ly) (Coord x y) 
  | directionCloture=="NS" = openEntity (openEntity gs "ClotureElectrique NS Ouvert" x y) "Levier Ferme" lx ly
  | directionCloture=="EO" = openEntity (openEntity gs "ClotureElectrique EO Ouvert" x y) "Levier Ferme" lx ly
 

-- Permet de donné l'effet de vibration lorsque le perso touche des piques
checkProjection :: GameState -> GameState
checkProjection gs@(GameState (Translation tx ty) _ sp (Perso px py d _ _) (Terrain ht lg carte) _) 
  | d == North && (checkCaseVide (Coord (coordonneesPx tx px 0) ((coordonneesPy (ty-25) py 0))) carte) =  gs {translate = (Translation tx (ty-8))}
  | d == North = gs {translate = (Translation tx (ty-sp))} -- J'utilise pas movedown sinon la direction change et ça nous emène dans une autre condition
  | d == South && (checkCaseVide (Coord (coordonneesPx tx px 0) ((coordonneesPy (ty+25) py 0))) carte)= gs {translate = (Translation tx (ty+8))}
  | d == South = gs {translate = (Translation tx (ty+sp))}
  | d == East && (checkCaseVide (Coord (coordonneesPx (tx+25) px 0) ((coordonneesPy ty py 0))) carte)= gs {translate = (Translation (tx+8) ty)}
  | d == East = gs {translate = (Translation (tx-sp) ty)}
  | d == West && (checkCaseVide (Coord (coordonneesPx (tx-25) px 0) ((coordonneesPy ty py 0))) carte) = gs {translate = (Translation (tx-8) ty)}
  | otherwise = gs {translate = (Translation (tx+sp) ty)}


-- | Fonctions qui gère la vie du perso
changePv :: GameState -> CInt -> GameState
changePv gs@(GameState _ _ _ (Perso px py d v inv) _ _) vie = let v'=v+vie in
                                                    if v' > 100
                                                      then gs {perso = (Perso px py d 100 inv)}
                                                      else if v' < 0 
                                                        then gs {perso = (Perso px py d 0 inv)}
                                                        else gs {perso = (Perso px py d v' inv)}

--verifie si la vie du perso à bien changé
changePv_post :: GameState -> CInt -> Bool
changePv_post gs@(GameState _ _ _ (Perso px py d v inv) _ _) vie = undefined

-- |IA
action :: GameState -> IO (GameState)
action gs@(GameState _ _ _ _ (Terrain  ht lg c) _) = do
  let entites = getCoordonneesObjectMap c (Just Zombie)
  (moveAll gs entites)

action_pre :: GameState -> Bool
action_pre gs@(GameState _ _ _ _ (Terrain  ht lg c) _) = testMap c "Zombie"

moveAll :: GameState -> [Coord] -> IO (GameState)
moveAll gs [] = return gs
moveAll gs (x:xs) = do
  newgs <- move gs x
  moveAll newgs xs

moveAll_pre :: GameState -> [Coord] -> Bool
moveAll_pre gs (Coord x y:xs) = (isitanEntity gs "Zombie" x y) /= ((-1 ::CInt),(-1 ::CInt))

move :: GameState -> Coord -> IO (GameState)
move gs@(GameState _ _ _ _ (Terrain  ht lg c) _) (Coord x y)= do
  alea <-randomRIO (1,4) :: IO Int
  case alea of
    1 -> if ((Map.lookup (Coord x (y-1)) c)) == Nothing 
          then  return gs { terrain = (Terrain ht lg (C.updateKeyMap (Coord x y) (Coord x (y-1)) c))} else return gs
    2 -> if ((Map.lookup (Coord x (y+2)) c)) == Nothing 
          then return gs { terrain = (Terrain ht lg (C.updateKeyMap (Coord x y) (Coord x (y+1)) c))} else return gs
    3 -> if ((Map.lookup (Coord (x+1) y) c)) == Nothing 
          then return gs { terrain = (Terrain ht lg (C.updateKeyMap (Coord x y) (Coord (x+1) y) c))} else return gs
    4 -> if ((Map.lookup (Coord (x-1) y) c)) == Nothing 
          then return gs { terrain = (Terrain ht lg (C.updateKeyMap (Coord x y) (Coord (x-1) y) c))} else return gs

move_pre :: GameState -> Coord -> Bool
move_pre gs (Coord x y) = x > 0 && y > 0

-- |Outils de debuguage

{------Concernant les valeurs fixes dans les collisions: 
-> ligne 56 : Le -4 permet d'éviter de se retrouver dans un mur. Les blocs sont de 20 pixel mais le perso se déplace de 4pixel, 
  on peut donc observer un ecart de 4 pixel qui fait rentrer une petite partie dans le mur.
  Consequences : On ne peux plus longer un mur du haut vers le bas
->ligne 61: Le 29 concerne un problème de sprite. Les sprites son positionnés avec des coordonées placé en haut à gauche (0,0). 
  La collision sera effective sur la tranche droite du bloc. 
  On comble se décalage tel que 29 = 25 (la largeur du personnage) + 4 (Le déplacement de 4 pixel)
  Consequences:  On ne peux plus longer un mur du haut vers le bas
->ligne 66: pareil que la ligne 56.
  Consequences : On ne peux plus longer un mur de l'est vers l'ouest
->ligne 71 : 24 = 20 (bloc du bac) + 4 (déplcement du personnage en pixel)

-}
-- |Coordonnees stricte personnage axe X (Aka les coordonnées en terme de case et pas de pixel)
coordonneesPx :: CInt -> CInt -> CInt -> CInt
coordonneesPx tx px x = (((px+x)-(fromIntegral tx))`div`20)

--On verifie seuelement que le personnage est au milieu de l'écran, px ne change jamais
coordonneesPx_pre :: CInt -> CInt -> CInt -> Bool
coordonneesPx_pre _ px _= px ==350

--Coordonnees stricte personnage axe Y (Aka les coordonnées en terme de case et pas de pixel)
coordonneesPy :: CInt -> CInt -> CInt -> CInt
coordonneesPy ty py y= (((py+25+y)-(fromIntegral ty))`div`20)

--On verifie seuelement que le personnage est au milieu de l'écran, py ne change jamais
coordonneesPy_pre :: CInt -> CInt -> CInt -> Bool
coordonneesPy_pre _ py _= py == 350

-- |Outils de debuguage
--renvoie la position du joueur ainsi que la valeur de la case associé à cette position
collision2 :: GameState -> IO ()
collision2 gs@(GameState (Translation tx ty) _ sp (Perso px py d _ inv) (Terrain  ht lg c) etatjeu) = do
  -- let door= isitaDoorRight gs (coordonneesPx (fromIntegral tx) px 29) (coordonneesPy (fromIntegral ty) py 0)
  --let clotures = (getCoordonneesObjectMap c (Just (ClotureElectrique NS Ouvert)))
  --let ((Coord x y):xs)= clotures
  -- let touttile=(collisionTileRight gs px py )
  -- let value= (Map.lookup (Coord (coordonneesPx (fromIntegral tx) px 29) (coordonneesPy (fromIntegral ty) py 0) ) c)
  --print ()
  --print ( "-------", door)
  print ("--------",(testLevier gs),d ,"-----------")

------------------------------------------------------------------------------------------------



-- |Mise a jour de l'était du jeu lors d'un déplacement

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime | (testSortie gstate) = gstate {etatjeu = Gagner}
                              | (tombeDansPiege gstate)= (testPiege gstate)
                              | (K.keypressed KeycodeE kbd) = (testLevier (testChest (testTresor (testDoor gstate))))
                              | (K.keypressed KeycodeA kbd) = (usePotion gstate)
                              | (K.keypressed KeycodeD kbd) && (K.keypressed KeycodeZ kbd) && (K.keypressed KeycodeQ kbd) = gstate
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
