module Carte where

import Foreign.C.Types (CInt)

import Data.Map 
import qualified Data.Map as M

data Statut = Ouvert |Ferme deriving (Eq,Show) --statut de la porte

data Direction =NS | EO deriving (Eq,Show) -- A changer plus tard

data Case = Vide
    | Porte Direction Statut --La porte est ouverte ou fermé
    | Mur
    | Coffre
    | Entree
    | Sortie
    deriving (Eq, Show)

data Coord = Coord { cx :: CInt, cy ::CInt} deriving (Eq,Show,Ord)

data Terrain = Terrain {ht :: CInt, lg ::CInt, contenu :: (M.Map Coord Case)} deriving (Show) --ht = hauteur , lg = largeur

initTerrain :: CInt -> CInt -> Map Coord Case -> Terrain
initTerrain ht lg contenu = Terrain ht lg contenu
--Initialise le terrain : Dans le loadTerrain du main, j'ajoute les infos du terrain ici


terrainGenerator :: FilePath -> IO (Terrain)
terrainGenerator fp = do
    file <- readFile fp --string
    let (contenu,ht,lg) = createTheMap file M.empty 0 0 0
    return (initTerrain ht lg contenu)


 
--Mutliplier les valeurs x et y par 20 (taille d'une case)

--Pour l'instant crée que les murs
createTheMap :: [Char] -> M.Map Coord Case -> CInt -> CInt -> CInt -> (M.Map Coord Case, CInt, CInt)
createTheMap [] mymap x y lg= (mymap, y, lg)
createTheMap (a:as) mymap x y lg | (a== '\n') && (as /= [])= createTheMap as mymap 0 (y+1) lg --lg bouge pas car ici c'est la condition du retour à la ligne
                                 | a=='x' = createTheMap as (M.insert (Coord x y) Mur mymap) (x+1) y (if lg< x then x else lg ) --Si on voit un mur
                                 | a=='c' = createTheMap as (M.insert (Coord x y) Coffre mymap) (x+1) y (if lg< x then x else lg ) --Si on voit un coffre
                                 | a== '-' = createTheMap as (M.insert (Coord x y) (Porte NS Ferme) mymap) (x+1) y (if lg< x then x else lg )
                                 | a== '|' = createTheMap as (M.insert (Coord x y) (Porte EO Ferme) mymap) (x+1) y (if lg< x then x else lg )
                                 |otherwise = createTheMap as mymap (x+1) y lg --lg bouge pas ici car c'est la conditions pour les espaces