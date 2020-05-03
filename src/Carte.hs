module Carte where

import Foreign.C.Types (CInt)

import Data.Map 
import qualified Data.Map as M

data Statut = Ouvert |Ferme deriving (Eq,Show) --statut de la porte

data Direction =NS | EO deriving (Eq,Show) -- A changer plus tard

data Case = Vide
    | Porte Direction Statut --La porte est ouverte ou fermé
    | Mur
    | Coffre Statut
    | Entree
    | Sortie
    | Zombie
    deriving (Eq, Show)

data Coord = Coord { cx :: CInt, cy ::CInt} deriving (Eq,Show,Ord)

data Terrain = Terrain {ht :: CInt, lg ::CInt, contenu :: (M.Map Coord Case)} deriving (Show) --ht = hauteur , lg = largeur

initTerrain :: CInt -> CInt -> Map Coord Case -> Terrain
initTerrain ht lg contenu = Terrain ht lg contenu
--Initialise le terrain : Dans le loadTerrain du main, j'ajoute les infos du terrain ici

initTerrain_pre :: CInt -> CInt -> Map Coord Case -> Bool
initTerrain_pre ht lg contenu | ht >0 && lg >0 && (length contenu) >0 = True
                              | otherwise = False

terrainGenerator :: FilePath -> IO (Terrain)
terrainGenerator fp = do
    file <- readFile fp --string
    let (contenu,ht,lg) = createTheMap file M.empty 0 0 0
    if (invariantCreateMap ht lg contenu) then 
        return (initTerrain ht lg contenu)
        else 
            return (initTerrain ht lg M.empty) -- Je retourne une map vide si l'invariant est faux

terrainGenerator_pre :: FilePath -> Bool
terrainGenerator_pre fp = undefined

--Mutliplier les valeurs x et y par 20 (taille d'une case)
createTheMap :: [Char] -> M.Map Coord Case -> CInt -> CInt -> CInt -> (M.Map Coord Case, CInt, CInt)
createTheMap [] mymap x y lg= (mymap, y, lg)
createTheMap (a:as) mymap x y lg | (a== '\n') && (as /= [])= createTheMap as mymap 0 (y+1) lg --lg bouge pas car ici c'est la condition du retour à la ligne
                                 | a=='x' = createTheMap as (M.insert (Coord x y) Mur mymap) (x+1) y (if lg< x then x else lg ) --Si on voit un mur
                                 | a=='c' = createTheMap as (M.insert (Coord x y) (Coffre Ferme) mymap) (x+1) y (if lg< x then x else lg ) --Si on voit un coffre
                                 | a== '-' = createTheMap as (M.insert (Coord x y) (Porte NS Ferme) mymap) (x+1) y (if lg< x then x else lg )
                                 | a== '|' = createTheMap as (M.insert (Coord x y) (Porte EO Ferme) mymap) (x+1) y (if lg< x then x else lg )
                                 | a== 'E' = createTheMap as (M.insert (Coord x y) (Entree) mymap) (x+1) y (if lg< x then x else lg )
                                 | a== 'z' = createTheMap as (M.insert (Coord x y) (Zombie) mymap) (x+1) y (if lg< x then x else lg )
                                 | a== 'S' = createTheMap as (M.insert (Coord x y) (Sortie) mymap) (x+1) y (if lg< x then x else lg )
                                 |otherwise = createTheMap as mymap (x+1) y lg --lg bouge pas ici car c'est la conditions pour les espaces

-- |Invariant pour la creation du terrain
invariantCreateMap:: CInt -> CInt -> M.Map Coord Case -> Bool
invariantCreateMap ht lg contenu = if (invariantObjets ht lg contenu) && (invariantMurs ht lg contenu) then True else False

-- |Verifie que les objects sont à l'intérieur du rectangle délimitant le donjon
invariantObjets:: CInt -> CInt -> M.Map Coord Case ->Bool
invariantObjets ht lg carte= let liste = M.keys $ filterWithKey (\k v -> (Just v)/=(Just Mur)) carte in
    aux ht lg liste where
        aux ht lg ((Coord x y):xs) | x<= 0 || y <=0 || x>=lg || y>=ht =False
                                   | xs==[] && (x>0 && y>0 && x<lg && y<ht) = True
                                   |otherwise = aux ht lg xs

-- | Verifie que les murs forment bien un rectangle.
invariantMurs::CInt -> CInt -> M.Map Coord Case ->Bool
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

-- |Fonction d'entree: Récupère l'entrée dans la carte pour pouvoir placer le joueur
getEntree::(Map Coord Case) -> Coord
getEntree carte = 
  let monentree = M.keys $ filterWithKey (\k v -> (Just v)==(Just Entree)) carte in
    if (invariantEntree monentree) then monentree!!0 else Coord (-1) (-1)

getSortie :: (Map Coord Case) -> Coord
getSortie carte = 
  let masortie = M.keys $ filterWithKey (\k v -> (Just v)==(Just Sortie)) carte in
    if (invariantSortie masortie) then masortie!!0 else Coord (-1) (-1)

invariantEntree::[Coord] -> Bool
invariantEntree coords | length coords == 1 = True 
                       | otherwise = False

invariantSortie :: [Coord] -> Bool
invariantSortie coords | length coords == 1 = True 
                       | otherwise = False

-- | Récuperer les coordonnées d'un objet en particlier dans la map (ex: tous les coffres ou tous les murs)
getCoordonneesObjectMap:: Eq a => (Map Coord a) -> Maybe a -> [Coord]
getCoordonneesObjectMap carte object= M.keys $ filterWithKey (\k v -> (Just v)==object) carte

getCoordonneesObjectMap_pre ::  Eq a => (Map Coord a) -> Maybe a -> Bool
getCoordonneesObjectMap_pre carte object=undefined

-- |Fonctions Utilitaires
--Met a jour la valeur d'un objet dans la carte du GameState
updateValueMap::(Map Coord Case) -> Coord -> Case -> (Map Coord Case)
updateValueMap carte coord unecase = let newmap = (M.insert coord unecase carte ) in newmap


updateValueMap_pre:: (Map Coord Case) -> Coord -> Case ->Bool
updateValueMap_pre carte coord unecase = undefined

--Met à jour la clé d'un objet dans la carte du GameState
updateKeyMap :: Coord -> Coord -> Map Coord Case -> Map Coord Case
updateKeyMap k0 k1 myMap = case M.lookup k0 myMap of
   Nothing -> myMap
   Just e  -> M.insert k1 e (M.delete k0 myMap)

-- On peux update la clé que dans le cas où la case est un zombie (Pour l'instant)
updateKeyMap_pre :: Coord -> Coord -> Map Coord Case -> Bool
updateKeyMap_pre (Coord x y) (Coord x1 y1) myMap | x >= 0 && y >=0 && x1>=0 && y1 >=0 && (M.lookup (Coord x y) myMap) == (Just Zombie)= True
                                                 |otherwise = False

--Retourne une Case associé a un objet de type String
getCaseFromString :: String -> Case
getCaseFromString entity | entity == "Coffre Ferme" = (Coffre Ouvert)
                         | entity == "Sortie" = Sortie
                         | otherwise = Vide 

getCaseFromString_pre :: String -> Bool
getCaseFromString_pre entity = undefined