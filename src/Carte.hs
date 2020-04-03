module Carte where

import Data.Map 
import qualified Data.Map as M

data Statut = Ouverte |Fermee deriving Eq --statut de la porte

data Direction =NS | EO deriving Eq -- A changer plus tard

data Case = Vide
    | Porte Direction Statut --La porte est ouverte ou fermé
    | Mur
    | Entree
    | Sortie
    deriving Eq

data Coord = Coord { cx :: Int, cy ::Int} deriving Eq

data Terrain = Terrain {ht :: Int, lg ::Int, contenu :: (M.Map Coord Case)} --ht = hauteur , lg = largeur

initTerrain :: Int -> Int -> Map Coord Case -> Terrain
initTerrain ht lg contenu = Terrain ht lg contenu
--Initialise le terrain : Dans le loadTerrain du main, j'ajoute les infos du terrain ici

--createTerrain:: TextureId -> Image
--createTerrain =