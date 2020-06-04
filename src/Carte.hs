module Carte where

import Foreign.C.Types (CInt)

import Data.Map 
import qualified Data.Map as M

data Statut = Ouvert | Ferme deriving (Eq,Show) --statut pour des portes, des coffres, des piques, des clotures, des leviers

data Direction = NS | EO deriving (Eq,Show) -- A changer plus tard

data Case = Vide
  | Porte Direction Statut --La porte est ouverte ou fermé
  | Mur
  | Coffre Statut
  | Tresor Statut
  | Entree
  | Sortie
  | Zombie
  | Pique Statut
  | ClotureElectrique Direction Statut
  | Levier Statut
  deriving (Eq, Show)

data Coord = Coord { cx :: CInt, cy :: CInt} deriving (Eq,Show,Ord)

data Terrain = Terrain {ht :: CInt, lg :: CInt, contenu :: (M.Map Coord Case)} 
  deriving (Show,Eq) --ht = hauteur , lg = largeur

initTerrain :: CInt -> CInt -> Map Coord Case -> Terrain
initTerrain ht lg contenu = Terrain ht lg contenu
--Initialise le terrain : Dans le loadTerrain du main, j'ajoute les infos du terrain ici

initTerrain_pre :: CInt -> CInt -> Map Coord Case -> Bool
initTerrain_pre ht lg contenu = ht >0 && lg >0 && (length contenu) >0

terrainGenerator :: FilePath -> IO (Terrain)
terrainGenerator fp = do
  file <- readFile fp --string
  let (contenu,ht,lg) = createTheMap file M.empty 0 0 0
  if (invariantCreateMap ht lg contenu) then 
    return (initTerrain ht lg contenu)
    else 
      return (initTerrain ht lg M.empty) -- Je retourne une map vide si l'invariant est faux

--Mutliplier les valeurs x et y par 20 (taille d'une case)
createTheMap :: [Char] -> M.Map Coord Case -> CInt -> CInt -> CInt -> (M.Map Coord Case, CInt, CInt)
createTheMap [] mymap x y lg= (mymap, y, lg)
createTheMap (a:as) mymap x y lg 
  | (a== '\n') && (as /= [])= createTheMap as mymap 0 (y+1) lg --lg bouge pas car ici c'est la condition du retour à la ligne
  | a=='x' = createTheMap as (M.insert (Coord x y) Mur mymap) (x+1) y (if lg< x then x else lg ) --Si on voit un mur
  | a=='c' = createTheMap as (M.insert (Coord x y) (Coffre Ferme) mymap) (x+1) y (if lg< x then x else lg ) --Si on voit un coffre
  | a=='t' = createTheMap as (M.insert (Coord x y) (Tresor Ferme) mymap) (x+1) y (if lg< x then x else lg ) --Si on voit un tresor
  | a== '-' = createTheMap as (M.insert (Coord x y) (Porte NS Ferme) mymap) (x+1) y (if lg< x then x else lg )
  | a== '|' = createTheMap as (M.insert (Coord x y) (Porte EO Ferme) mymap) (x+1) y (if lg< x then x else lg )
  | a== 'E' = createTheMap as (M.insert (Coord x y) (Entree) mymap) (x+1) y (if lg< x then x else lg )
  | a== 'z' = createTheMap as (M.insert (Coord x y) (Zombie) mymap) (x+1) y (if lg< x then x else lg )
  | a== 'S' = createTheMap as (M.insert (Coord x y) (Sortie) mymap) (x+1) y (if lg< x then x else lg )
  | a== 'p' = createTheMap as (M.insert (Coord x y) (Pique Ferme) mymap) (x+1) y (if lg< x then x else lg )
  | a== 'w' = createTheMap as (M.insert (Coord x y) (ClotureElectrique NS Ouvert) mymap) (x+1) y (if lg< x then x else lg )
  | a== 'k' = createTheMap as (M.insert (Coord x y) (ClotureElectrique EO Ouvert) mymap) (x+1) y (if lg< x then x else lg )
  | a== 'l' = createTheMap as (M.insert (Coord x y) (Levier Ferme) mymap) (x+1) y (if lg< x then x else lg )
  |otherwise = createTheMap as mymap (x+1) y lg --lg bouge pas ici car c'est la conditions pour les espaces

-- |Invariant pour la creation du terrain
invariantCreateMap :: CInt -> CInt -> M.Map Coord Case -> Bool
invariantCreateMap ht lg contenu = if (invariantObjets ht lg contenu) && (invariantMurs ht lg contenu) then True else False

-- |Verifie que les objects sont à l'intérieur du rectangle délimitant le donjon
invariantObjets :: CInt -> CInt -> M.Map Coord Case -> Bool
invariantObjets ht lg carte= let liste = M.keys $ filterWithKey (\k v -> (Just v)/=(Just Mur)) carte in
  aux ht lg liste where
    aux ht lg ((Coord x y):xs) | x<= 0 || y <=0 || x>=lg || y>=ht =False
                               | xs==[] && (x>0 && y>0 && x<lg && y<ht) = True
                               | otherwise = aux ht lg xs

-- | Verifie que les murs forment bien un rectangle.
invariantMurs :: CInt -> CInt -> M.Map Coord Case -> Bool
invariantMurs ht lg contenu= if (invmurshorizontals contenu 0 0 lg) && (invmurshorizontals contenu 0 ht lg) 
                                && (invmursvertical contenu 0 0 ht) && (invmursvertical contenu lg 0 ht)
                                  then True
                                  else False
                                        
        
invmurshorizontals:: M.Map Coord Case -> CInt -> CInt -> CInt -> Bool
invmurshorizontals carte x y lg | x==lg = True
                                | M.lookup (Coord x y) carte == (Just Mur) = invmurshorizontals carte (x+1) y lg
                                | otherwise = False

invmursvertical:: M.Map Coord Case -> CInt -> CInt -> CInt -> Bool
invmursvertical carte x y ht | y==ht = True
                             | M.lookup (Coord x y) carte == (Just Mur) = invmursvertical carte x (y+1) ht
                             | otherwise = False

-- |Verifie que les portes sont rentre 2 murs
checkPorte :: (Map Coord Case) -> Bool
checkPorte carte  = 
     let mesportes = M.keys $ filterWithKey (\k v -> ((Just v) == (Just (Porte NS Ferme)) 
                                                   || (Just v) == (Just (Porte EO Ferme))
                                                   || (Just v) == (Just (Porte NS Ouvert))
                                                   || (Just v) == (Just (Porte EO Ouvert)))) carte in 
                                                      if (length mesportes)>0 then auxcheckPortes mesportes carte else True

auxcheckPortes :: [Coord] -> (Map Coord Case) -> Bool
auxcheckPortes ((Coord x y):xs) carte 
  | ((objectOnPosition carte x y) == "Porte NS") && ((objectOnPosition carte (x-1) y) == "Mur") && ((objectOnPosition carte (x+1) y) == "Mur")  && xs == [] = True
  | ((objectOnPosition carte x y) == "Porte EO") && ((objectOnPosition carte x (y-1)) == "Mur") && ((objectOnPosition carte x (y+1)) == "Mur")  && xs == [] = True
  | ((objectOnPosition carte x y) == "Porte EO") && ((objectOnPosition carte x (y-1)) == "Mur") && ((objectOnPosition carte x (y+1)) == "Mur") = auxcheckPortes xs carte
  | ((objectOnPosition carte x y) == "Porte NS") && ((objectOnPosition carte (x-1) y) == "Mur") && ((objectOnPosition carte (x+1) y) == "Mur") = auxcheckPortes xs carte
  | otherwise = False

--Verifie s'il y'a un seul trésor 
checkTresor :: (Map Coord Case) -> Bool
checkTresor carte = 
  let tresor = M.keys $ filterWithKey (\k v -> (Just v) == (Just (Tresor Ferme))) carte in 
                                                      if (length tresor)==1 then True else False

--Verifie si il existe un levier par cloture
checkLevier :: (Map Coord Case) -> Bool
checkLevier carte =
  let levier = M.keys $ filterWithKey (\k v -> (Just v) == Just (Levier Ferme)) carte in
    let piege = M.keys $ filterWithKey (\k v -> ((Just v) == Just (ClotureElectrique NS Ouvert)||
                                                 (Just v) == Just (ClotureElectrique EO Ouvert))) carte in
      if (length levier == length piege) then True else False

-- |Fonction d'entree: Récupère l'entrée dans la carte pour pouvoir placer le joueur

getEntree :: (Map Coord Case) -> Coord
getEntree carte = 
  let monentree = M.keys $ filterWithKey (\k v -> (Just v)==(Just Entree)) carte in
    if (invariantEntree monentree) then monentree!!0 else Coord (-1) (-1)

getSortie :: (Map Coord Case) -> Coord
getSortie carte = 
  let masortie = M.keys $ filterWithKey (\k v -> (Just v)==(Just Sortie)) carte in
    if (invariantSortie masortie) then masortie!!0 else Coord (-1) (-1)

invariantEntree :: [Coord] -> Bool
invariantEntree coords = length coords == 1

invariantSortie :: [Coord] -> Bool
invariantSortie coords = length coords == 1


-- |Fonctions de recherche

objectOnPosition :: (Map Coord Case) -> CInt -> CInt -> String
objectOnPosition c x y = (case (M.lookup (Coord x y) c) of
                                                Just Mur -> "Mur"
                                                Just (Coffre Ouvert) -> "Coffre Ouvert"
                                                Just (Coffre Ferme) -> "Coffre Ferme"
                                                Just (Tresor Ouvert) -> "Tresor Ouvert"
                                                Just (Tresor Ferme) -> "Tresor Ferme"
                                                Just (Porte NS Ferme) -> "Porte NS"
                                                Just (Porte EO Ferme) -> "Porte EO"
                                                Just (Porte EO Ouvert) -> "Porte EO"
                                                Just (Porte NS Ouvert) -> "Porte NS"
                                                Just (ClotureElectrique NS Ouvert) -> "ClotureElectrique NS Ouvert"
                                                Just (ClotureElectrique NS Ferme) -> "ClotureElectrique NS Ferme"
                                                Just (ClotureElectrique EO Ouvert) -> "ClotureElectrique EO Ouvert"
                                                Just (ClotureElectrique EO Ferme) -> "ClotureElectrique EO Ferme"
                                                Just Entree -> "Entree"
                                                Just Sortie -> "Sortie"
                                                Just Zombie -> "Zombie"
                                                Just (Pique Ouvert) -> "Pique Ouvert"
                                                Just (Pique Ferme) -> "Pique Ferme"
                                                Just (Levier Ferme) -> "Levier Ferme"
                                                Just (Levier Ouvert) -> "Levier Ouvert"
                                                Nothing -> "Nothing")

collision :: (Map Coord Case) -> CInt -> CInt -> Bool
collision carte x y = (case (M.lookup (Coord x y) carte) of
                                                Just Mur -> True
                                                Just (Coffre Ouvert) -> True
                                                Just (Coffre Ferme) -> True
                                                Just (Tresor Ouvert) -> True
                                                Just (Tresor Ferme) -> True
                                                Just (Porte NS Ferme) -> True
                                                Just (Porte NS Ouvert) -> False
                                                Just (Porte EO Ferme) -> True
                                                Just (Porte EO Ouvert) -> False
                                                Just (ClotureElectrique NS Ouvert) -> False
                                                Just (ClotureElectrique NS Ferme) -> False
                                                Just (ClotureElectrique EO Ouvert) -> False
                                                Just (ClotureElectrique EO Ferme) -> False
                                                Just Entree -> False
                                                Just Sortie -> False
                                                Just Zombie -> True
                                                Just (Pique Ouvert) -> False
                                                Just (Pique Ferme) -> False
                                                Just (Levier Ferme) -> True
                                                Just (Levier Ouvert) -> True
                                                Nothing -> False)

--Verifie que la carte n'est pas vide et que x et y sont positif 
--(annoding peut-être mais c'est pour garder une certaine cohérence -> les coordonnées sont toutes positives)
collision_pre :: (Map Coord Case) ->CInt -> CInt -> Bool
collision_pre carte x y = (length carte) > 0 && x >=0 && y>=0

-- Pas sûr : A verfier 
collision_post :: (Map Coord Case) ->CInt -> CInt -> Bool
collision_post carte x y = (M.lookup (Coord x y) carte) /= Nothing

-- | Récuperer toutes les coordonnées d'un objet en particlier dans la map (ex: tous les coffres ou tous les murs)
getCoordonneesObjectMap :: (Map Coord Case) -> Maybe Case -> [Coord]
getCoordonneesObjectMap carte object= M.keys $ filterWithKey (\k v -> (Just v)==object) carte

-- Verifie si carte non vide et l'objet est un objet existant
getCoordonneesObjectMap_pre :: (Map Coord Case) -> Maybe Case -> Bool
getCoordonneesObjectMap_pre carte object = (length carte) > 0 && (case object of
                                                                    Just Mur -> True
                                                                    Just (Coffre Ouvert) -> True
                                                                    Just (Coffre Ferme) -> True
                                                                    Just (Tresor Ouvert) -> True
                                                                    Just (Tresor Ferme) -> True
                                                                    Just (Porte NS Ferme) -> True
                                                                    Just (Porte NS Ouvert) -> False
                                                                    Just (Porte EO Ferme) -> True
                                                                    Just (Porte EO Ouvert) -> False
                                                                    Just (ClotureElectrique NS Ouvert) -> False
                                                                    Just (ClotureElectrique NS Ferme) -> False
                                                                    Just (ClotureElectrique EO Ouvert) -> False
                                                                    Just (ClotureElectrique EO Ferme) -> False
                                                                    Just Entree -> False
                                                                    Just Sortie -> False
                                                                    Just Zombie -> True
                                                                    Just (Pique Ouvert) -> False
                                                                    Just (Pique Ferme) -> False
                                                                    Just (Levier Ouvert) -> True
                                                                    Just (Levier Ferme) -> True
                                                                    Nothing -> False) 

-- | Test d'entite dans toute la map pour les preconditions
testMap :: Map Coord Case -> String -> Bool
testMap c entity = M.foldr (\x b -> b || x == (getCaseFromString entity) ) False c


-- |Fonctions Utilitaires
-- Met a jour la valeur d'un objet dans la carte du GameState
updateValueMap :: (Map Coord Case) -> Coord -> Case -> (Map Coord Case)
updateValueMap carte coord unecase = let newmap = (M.insert coord unecase carte ) in newmap

updateValueMap_post :: (Map Coord Case) -> Coord -> Case -> Bool
updateValueMap_post carte coord unecase = (M.lookup coord carte) == (Just unecase)

--Il faut que les coordonnées soit inf à ht et lg aussi
updateValueMap_pre :: (Map Coord Case) -> Coord -> Case -> Bool
updateValueMap_pre carte (Coord x y) unecase = x>=0 && y>=0

--Met à jour la clé d'un objet dans la carte du GameState
updateKeyMap :: Coord -> Coord -> Map Coord Case -> Map Coord Case
updateKeyMap k0 k1 myMap = case M.lookup k0 myMap of
   Nothing -> myMap
   Just e  -> M.insert k1 e (M.delete k0 myMap)

-- On peux update la clé que dans le cas où la case est un zombie (Pour l'instant)
-- il faut que les coordonnes inf à la ht et lg
updateKeyMap_pre :: Coord -> Coord -> Map Coord Case -> Bool
updateKeyMap_pre (Coord x y) (Coord x1 y1) myMap = x >= 0 && y >=0 && x1>=0 && y1 >=0

--Si la coordonnées de l'objet à changer
updateKeyMap_post :: Coord -> Coord -> Map Coord Case -> Bool
updateKeyMap_post before after myMap = ((M.lookup before myMap) == Nothing) && ((M.lookup after myMap) /= Nothing)

--Retourne une Case associé a un objet de type String
-- Principalement utilisé pour factorisé le code du model
--Les valeur du coffre et pique sont inversé (SI ferme on renvoie ouvert et vis versa) pour effectuer un changement plus facilement ? Ou pas
getCaseFromString :: String -> Case
getCaseFromString entity | entity == "Coffre Ferme" = (Coffre Ouvert)
                         | entity == "Coffre Ouvert" = (Coffre Ferme)  
                         | entity == "Tresor Ferme" = (Tresor Ouvert)       
                         | entity == "Tresor Ouvert" = (Tresor Ferme)       
                         | entity == "Sortie" = Sortie
                         | entity == "Zombie" = Zombie
                         | entity == "Entree" = Entree
                         | entity == "Mur" = Mur
                         | entity == "Porte NS Ferme" = Porte NS Ferme 
                         | entity == "Porte NS Ouvert" = Porte NS Ouvert
                         | entity == "Porte EO Ferme" = Porte EO Ferme
                         | entity == "Porte EO Ouvert" = Porte EO Ouvert
                         | entity == "Pique Ouvert" = Pique Ferme
                         | entity == "Pique Ferme" = Pique Ouvert
                         | entity == "ClotureElectrique NS Ouvert" = ClotureElectrique NS Ferme
                         | entity == "ClotureElectrique NS Ferme" = ClotureElectrique NS Ouvert
                         | entity == "ClotureElectrique EO Ouvert" = ClotureElectrique EO Ferme
                         | entity == "ClotureElectrique EO Ferme" = ClotureElectrique EO Ouvert
                         | entity == "Levier Ouvert" = Levier Ferme
                         | entity == "Levier Ferme" = Levier Ouvert
                         | otherwise = Vide 

--Si resultat non vide c'est true sinon false
--A REVOIR
getCaseFromString_post :: String -> Bool
getCaseFromString_post entity = entity == "Coffre Ferme" || entity == "Sortie"

--Verifie si la carte est valide
carteValide :: (Map Coord Case) -> Bool
carteValide carte = ((getEntree carte)/= (Coord (-1) (-1)) ) &&  ((getSortie carte)/= (Coord (-1) (-1))) && checkPorte carte && checkTresor carte && checkLevier carte

--Check si la case est vide
checkCaseVide :: Coord -> (Map Coord Case) -> Bool
checkCaseVide coord carte  = (M.lookup coord carte) == Nothing


distance :: Coord -> Coord -> CInt
distance (Coord x1 y1) (Coord x2 y2) = round (sqrt (x'*x'+y'*y'))
                                          where
                                            x'= fromIntegral (x1 - x2)
                                            y'= fromIntegral (y1 - y2)