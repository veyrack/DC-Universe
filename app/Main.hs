{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M

import Control.Monad.IO.Class
import Data.StateVar

import System.Random
import Data.Map
import qualified Data.Map.Strict as Map

import Carte
import qualified Carte as C

import Control.Exception
import Exception
import qualified Exception as E

import SDL.Video.Renderer (Renderer, Texture, Rectangle (..))
import qualified SDL.Video.Renderer as R


--Screen size
hauteurWin :: CInt
hauteurWin = 700

largeurWin:: CInt
largeurWin = 800

--size of the dungeon
{-
hauteurDj :: CInt
hauteurDj = 700

largeurDj:: CInt
largeurDj = 700


--Nombre de blocs en hauteur
blocHauteur::CInt
blocHauteur = hauteurDj `div` 20 --20pixel pour un bloc

--Nombre de blocs en largeur
blocLargeur::CInt
blocLargeur = largeurDj `div` 20 --20pixel pour un bloc
-}
--Position du personnage
persoX::CInt
persoX = 350

persoY::CInt
persoY = 350

tailleBloc:: CInt
tailleBloc = 20

--Renvoie la fenêtre de l'écran
getWindow:: MonadIO m => m Window
getWindow = createWindow "Dungeon Crawler Universe" $ defaultWindow { windowInitialSize = V2 largeurWin hauteurWin}

--sizeWindows :: Window -> IO (CInt, CInt)
--sizeWindows win= do
  --stateValue <- get (windowSize win)
  --let (V2 x y) = stateValue
  --return (x, y)

--Charge le personnage contrôlé par l'utilisateur 
loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 25 45)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

--Charge le sol du donjon (tous les blocs sont à la position 0 0)
loadSol:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadSol rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("sol")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("sol")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("sol")) sprite smap
  return (tmap', smap')

--Charge les murs du donjon (tous les blocs sont à la position 0)
loadMurs:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadMurs rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("mur")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("mur")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("mur")) sprite smap
  return (tmap', smap')


--Charge les coffres
loadCoffreFerme:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadCoffreFerme rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("coffreF")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("coffreF")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("coffreF")) sprite smap
  return (tmap', smap')
  
--Charge les coffres
loadCoffreOuvert:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadCoffreOuvert rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("coffreO")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("coffreO")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("coffreO")) sprite smap
  return (tmap', smap')

--charge les portes ferme
loadPorteFerme:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadPorteFerme rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("porteferme")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("porteferme")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("porteferme")) sprite smap
  return (tmap', smap')

--charge les portes ouvertes
loadPorteOuvert:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadPorteOuvert rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("porteouvert")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("porteouvert")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("porteouvert")) sprite smap
  return (tmap', smap')

--charge les pique ouvert
loadPiqueOuvert:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadPiqueOuvert rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("piqueouvert")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("piqueouvert")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("piqueouvert")) sprite smap
  return (tmap', smap')

--charge les pique ferme
loadPiqueFerme:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadPiqueFerme rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("piqueferme")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("piqueferme")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("piqueferme")) sprite smap
  return (tmap', smap')
  
--charge le text du titre
loadTextTitle:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadTextTitle rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("text")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("text")) (S.mkArea 0 0 (tailleBloc*30) (tailleBloc*10)) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("text")) sprite smap
  return (tmap', smap')

--charge le text de victoire
loadTextWin:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadTextWin rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("win")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("win")) (S.mkArea 0 0 (tailleBloc*30) (tailleBloc*20)) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("win")) sprite smap
  return (tmap', smap')
--Charge le texte de défaite
loadTextLose:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadTextLose rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("lose")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("lose")) (S.mkArea 0 0 (tailleBloc*30) (tailleBloc*30)) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("lose")) sprite smap
  return (tmap', smap')

--Charge la sortie
loadSortie:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadSortie rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("sortie")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("sortie")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("sortie")) sprite smap
  return (tmap', smap')

--Charge les IA
loadMob:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadMob rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("mob")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("mob")) (S.mkArea 0 0 25 35) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("mob")) sprite smap
  return (tmap', smap')

--charge les cloture electrique
loadClotureElectriqueOuvertNS:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadClotureElectriqueOuvertNS rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("ClotureElectriqueOuvertNS")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("ClotureElectriqueOuvertNS")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("ClotureElectriqueOuvertNS")) sprite smap
  return (tmap', smap')

loadClotureElectriqueFermeNS:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadClotureElectriqueFermeNS rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("ClotureElectriqueFermeNS")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("ClotureElectriqueFermeNS")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("ClotureElectriqueFermeNS")) sprite smap
  return (tmap', smap')

loadLevierOuvert:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadLevierOuvert rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("levierOuvert")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("levierOuvert")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("levierOuvert")) sprite smap
  return (tmap', smap')

loadLevierFerme:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadLevierFerme rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("levierFerme")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("levierFerme")) (S.mkArea 0 0 tailleBloc tailleBloc) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("levierFerme")) sprite smap
  return (tmap', smap')
-----------------------A la creation je place mes blocs ----------------------------------

--Affiche tous les blocs
displayBackground:: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> CInt -> Map Coord Case -> IO ()
displayBackground renderer tmap smap cpt ht lg transx trany carte= do
  displaySol renderer tmap smap 0 0 ht lg transx trany --display le sol
  displayMurs renderer tmap smap carte transx trany--display murs
  displayCoffreOuvert renderer tmap smap carte transx trany--display coffres
  displayCoffreFerme renderer tmap smap carte transx trany--display coffres
  displayPorteFerme renderer tmap smap carte transx trany --display des portes fermee
  displayPorteOuvert renderer tmap smap carte transx trany --display des portes ouvertes
  displayMob renderer tmap smap carte transx trany --display les ennemis
  displaySortie renderer tmap smap carte transx trany --display la sortie
  displayPiqueOuvert renderer tmap smap carte transx trany --display pique invisible
  displayPiqueFerme renderer tmap smap carte transx trany --display pique visible
  displayClotureElectriqueOuvertNS renderer tmap smap carte transx trany --display Cloture Electrique allumer
  displayClotureElectriqueFermeNS renderer tmap smap carte transx trany --display Cloture Electrique éteint
  displayLevierOuvert renderer tmap smap carte transx trany --display du levier ON
  displayLevierFerme renderer tmap smap carte transx trany --display du levier Off
  return ()
  --display portes

--Affiche le sol
displaySol :: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
displaySol renderer tmap smap posx posy ht lg transx transy= do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("sol")) smap) (posx+transx) (posy+transy))
  if posx < (lg-tailleBloc) then (displaySol renderer tmap smap (posx+tailleBloc) posy  ht lg transx transy ) 
    else if posy < (ht-tailleBloc) then (displaySol renderer tmap smap 0 (posy+tailleBloc)  ht lg transx transy)
      else return ()

--Affiche les murs

--Affiche les murs
displayMurs::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayMurs renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just Mur)) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("mur")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

--Affiches les coffres
displayCoffreFerme::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayCoffreFerme renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (Coffre Ferme))) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("coffreF")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as
                              
displayCoffreOuvert::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayCoffreOuvert renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (Coffre Ouvert))) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("coffreO")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

displayPorteFerme::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayPorteFerme renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (Porte EO Ferme) ) || (Just v)==(Just (Porte NS Ferme) )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("porteferme")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

displayPorteOuvert::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayPorteOuvert renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (Porte EO Ouvert) ) || (Just v)==(Just (Porte NS Ouvert) )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("porteouvert")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

displayPiqueOuvert::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayPiqueOuvert renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (Pique Ouvert) )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("piqueouvert")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

displayPiqueFerme::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayPiqueFerme renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (Pique Ferme) )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("piqueferme")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

displayMob::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayMob renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just Zombie )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("mob")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

displayClotureElectriqueOuvertNS :: Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayClotureElectriqueOuvertNS renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (ClotureElectrique NS Ouvert) )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("ClotureElectriqueOuvertNS")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

displayClotureElectriqueFermeNS :: Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayClotureElectriqueFermeNS renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (ClotureElectrique NS Ferme) )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("ClotureElectriqueFermeNS")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

displaySortie::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displaySortie renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just Sortie )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("sortie")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

displayLevierOuvert::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayLevierOuvert renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (Levier Ouvert) )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("levierOuvert")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as
displayLevierFerme::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayLevierFerme renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (Levier Ferme) )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("levierFerme")) smap) ((x*tailleBloc)+transx) ((y*tailleBloc)+transy))
                              test as

displayDebug:: Renderer -> IO ()
displayDebug renderer = do
  let color = V4 255 0 0 0
  rendererDrawColor renderer $= color
  let rectangle = drawRect renderer (Just (S.mkArea (persoX) (persoY+25) 25 20)) in rectangle

displayVie :: Renderer-> CInt -> IO ()
displayVie renderer vie= do
  let life= (vie*40) `div` 100
  let rectangle = drawRect renderer (Just (S.mkArea (persoX-10) persoY 40 10)) in rectangle
  let fillrect = fillRect renderer (Just (S.mkArea (persoX-10) persoY life 10)) in fillrect
--------------------------------------

main :: IO ()
main = do
  initializeAll
  window <- getWindow
  renderer <- createRenderer window (-1) defaultRenderer
  --chargement des ressources
  (kbd,tmap',smap', gameState) <- chargementRessources renderer
  -- |Menu du jeu
  title renderer kbd tmap' smap' gameState

chargementRessources:: Renderer -> IO (Keyboard,TextureMap,SpriteMap,GameState)
chargementRessources renderer= do
  --Generation d'un terrain à partir d'un fichier
  terrain <-terrainGenerator "CarteGenerator/carte.txt"
  let (Terrain ht lg contenu)= terrain
  --print (terrain)
  --Test si la map est valide
  if (not $ carteValide contenu)
    then throw InvalidMapException else putStrLn "Carte : Pass"
    
  -- chargement du sol
  (tmap, smap) <- loadSol renderer "assets/sol.png" TM.createTextureMap SM.createSpriteMap
  --chargement des murs
  (tmap,smap) <- loadMurs renderer "assets/murtest.png"  tmap smap--Ici, il faudra une fonction pour optenir le nombre de mur dans la map
  --chargement des coffres
  (tmap, smap) <- loadCoffreOuvert renderer "assets/coffreAssets/chest_empty_open_anim_f2.png" tmap smap
  (tmap, smap) <- loadCoffreFerme renderer "assets/coffre.png" tmap smap
  --chargement des portes
  (tmap, smap) <- loadPorteOuvert renderer "assets/porteouvert.png" tmap smap
  (tmap, smap) <- loadPorteFerme renderer "assets/porteferme.png" tmap smap
  --chargement ennemis
  (tmap, smap) <- loadMob renderer "assets/zombie2.png" tmap smap
  --chargement de la sortie
  (tmap, smap) <- loadSortie renderer "assets/sortie.png" tmap smap
  --charge les pieges
  (tmap, smap) <- loadPiqueFerme renderer "assets/PiegeAsset/piqueferme.png" tmap smap
  (tmap, smap) <- loadPiqueOuvert renderer "assets/PiegeAsset/piqueouvert.png" tmap smap
  --charge les clotures electrique
  (tmap, smap) <- loadClotureElectriqueOuvertNS renderer "assets/ClotureElectrique/ClotureElectriqueOuvertNS.png" tmap smap
  (tmap, smap) <- loadClotureElectriqueFermeNS renderer "assets/ClotureElectrique/ClotureElectriqueFermeNS.png" tmap smap
  --Chargement des leviers
  (tmap, smap) <- loadLevierOuvert renderer "assets/Levier/leverOn.png" tmap smap
  (tmap, smap) <- loadLevierFerme renderer "assets/Levier/leverOff.png" tmap smap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.png" tmap smap
  --chargement texte ecran titre
  (tmap', smap') <- loadTextTitle renderer "assets/title.png" tmap' smap'
  --chargement texte ecran win
  (tmap', smap') <- loadTextWin renderer "assets/youwin2.png" tmap' smap'
  --chargemen texte ecran loose
  (tmap', smap') <- loadTextLose renderer "assets/youlose.png" tmap' smap'

  -- initialisation de l'état du jeu
  let (Coord coorda coordb)= C.getEntree contenu
  let gameState = M.initGameState (M.Translation (persoX - (coorda*tailleBloc)) ((persoY+25) - (coordb*tailleBloc))) (M.Perso persoX persoY M.North 100) terrain --px et py sont les coordonnées de la map placé sur l'écran
  
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  return (kbd,tmap',smap',gameState)

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState@(M.GameState (M.Translation tx ty) tour sp (M.Perso px py d vie) (Terrain  ht lg contenu) etatjeu) = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  --print("JEU")
  clear renderer
  
  
  --- display toutes les couches du background
  displayBackground renderer tmap smap 0 (ht*tailleBloc) (lg*tailleBloc) (fromIntegral (tx)) (fromIntegral (ty)) contenu

  --Représente la vie du personnage
  displayVie renderer vie


  --- display perso 
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 persoX
                                persoY)

  --Test l'état du jeu
  if (etatjeu == M.Gagner) then youwin renderer kbd tmap smap gameState else return ()
  if (vie == 0) then youlose renderer kbd tmap smap gameState else return ()
  
  -- |Display debug
  displayDebug renderer
  --print (M.testSortie gameState)
  M.collision2 gameState
  --print (contenu)
  --print (gameState)

--Background Color
  let color = V4 0 0 0 0
  rendererDrawColor renderer $= color

  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  --putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  --putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state----

  --Deplacement mob = mise a jour de leurs positions
  newgs <- if ((tour `mod` 10)==0) then (M.action gameState) else return gameState

  let gameState'' = M.gameStep (newgs {M.tour = tour+1}) kbd' deltaTime
  ---------------------------
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState'')


title :: Renderer -> Keyboard -> TextureMap -> SpriteMap -> GameState -> IO ()
title renderer kbd tmap smap gs = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer
  --print ("TITLE")
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                persoX
                                persoY)
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "text") smap)
                                100
                                100)
  
  if ((K.keypressed KeycodeReturn kbd')) 
    then (gameLoop 60 renderer tmap smap kbd' gs)
    else return () 

  present renderer
  
  endTime <- time
  unless (K.keypressed KeycodeReturn kbd') (title renderer kbd tmap smap gs)

--Ecran de victoire
youwin :: Renderer -> Keyboard -> TextureMap -> SpriteMap -> GameState -> IO ()
youwin renderer kbd tmap smap gs = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer

  displaySol renderer tmap smap 0 0 hauteurWin largeurWin 0 0
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "win") smap)
                                100
                                100)                
  
  if ((K.keypressed KeycodeR kbd')) 
    then do
      (keyb,tmap',smap', gameState) <- chargementRessources renderer 
      (gameLoop 60 renderer tmap' smap' keyb gameState)
    else return () 

  present renderer  

  endTime <- time
  unless (K.keypressed KeycodeReturn kbd) (youwin renderer kbd tmap smap gs)

--Ecran de défaite
youlose :: Renderer -> Keyboard -> TextureMap -> SpriteMap -> GameState -> IO ()
youlose renderer kbd tmap smap gs = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer

  displaySol renderer tmap smap 0 0 hauteurWin largeurWin 0 0
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "lose") smap)
                                100
                                50)
  
  if ((K.keypressed KeycodeR kbd')) 
    then do
      (keyb,tmap',smap', gameState) <- chargementRessources renderer 
      (gameLoop 60 renderer tmap' smap' keyb gameState)
    else return () 

  present renderer
  endTime <- time
  unless (K.keypressed KeycodeReturn kbd') (youlose renderer kbd tmap smap gs)