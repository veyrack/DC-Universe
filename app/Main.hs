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

--Screen size
hauteurWin :: CInt
hauteurWin = 700

largeurWin:: CInt
largeurWin = 800

--size of the dungeon
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

--Position du personnage
persoX::CInt
persoX = largeurDj `div` 2

persoY::CInt
persoY = hauteurDj `div` 2

--Renvoie la fenêtre de l'écran
getWindow:: MonadIO m => m Window
getWindow = createWindow "Dungeon Crawler Universe" $ defaultWindow { windowInitialSize = V2 largeurWin hauteurWin}

--sizeWindows :: Window -> IO (CInt, CInt)
--sizeWindows win= do
  --stateValue <- get (windowSize win)
  --let (V2 x y) = stateValue
  --return (x, y)


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
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("sol")) (S.mkArea 0 0 20 20) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("sol")) sprite smap
  return (tmap', smap')

--Charge les murs du donjon (tous les blocs sont à la position 0)
loadMurs:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadMurs rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("mur")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("mur")) (S.mkArea 0 0 20 20) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("mur")) sprite smap
  return (tmap', smap')


--Charge les coffres
loadCoffre:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadCoffre rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("coffre")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("coffre")) (S.mkArea 0 0 20 20) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("coffre")) sprite smap
  return (tmap', smap')

--charge les portes ferme
loadPorteFerme:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadPorteFerme rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("porteferme")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("porteferme")) (S.mkArea 0 0 20 20) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("porteferme")) sprite smap
  return (tmap', smap')

--charge les portes ouvertes
loadPorteOuvert:: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadPorteOuvert rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId ("porteouvert")) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("porteouvert")) (S.mkArea 0 0 20 20) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("porteouvert")) sprite smap
  return (tmap', smap')

-----------------------A la creation je place mes blocs ----------------------------------

--Affiche tous les blocs
displayBackground:: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> CInt -> Map Coord Case -> IO ()
displayBackground renderer tmap smap cpt ht lg transx trany carte= do
  displaySol renderer tmap smap 0 0 ht lg transx trany --display le sol
  displayMurs renderer tmap smap carte transx trany--display murs
  displayCoffre renderer tmap smap carte transx trany--display coffres
  displayPorteFerme renderer tmap smap carte transx trany --display des portes fermee
  displayPorteOuvert renderer tmap smap carte transx trany --display des portes ouvertes
  return ()
  --display portes

--Affiche le sol
displaySol :: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
displaySol renderer tmap smap posx posy ht lg transx transy= do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("sol")) smap) (posx+transx) (posy+transy))
  if posx < (lg-20) then (displaySol renderer tmap smap (posx+20) posy  ht lg transx transy ) 
    else if posy < (ht-20) then (displaySol renderer tmap smap 0 (posy+20)  ht lg transx transy)
      else return ()

--Affiche les murs

--Affiche les murs
displayMurs::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayMurs renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just Mur)) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("mur")) smap) ((x*20)+transx) ((y*20)+transy))
                              test as

--Affiches les coffres
displayCoffre::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayCoffre renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just Coffre)) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("coffre")) smap) ((x*20)+transx) ((y*20)+transy))
                              test as

displayPorteFerme::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayPorteFerme renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (Porte EO Ferme) ) || (Just v)==(Just (Porte NS Ferme) )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("porteferme")) smap) ((x*20)+transx) ((y*20)+transy))
                              test as

displayPorteOuvert::Renderer->TextureMap -> SpriteMap -> Map Coord Case -> CInt -> CInt -> IO ()
displayPorteOuvert renderer tmap smap carte transx transy= do
  let mylist = Map.keys $ filterWithKey (\k v -> (Just v)==(Just (Porte EO Ouvert) ) || (Just v)==(Just (Porte NS Ouvert) )) carte
  test mylist where
    test [] = return ()
    test ((Coord x y):as) = do 
                              S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("porteouvert")) smap) ((x*20)+transx) ((y*20)+transy))
                              test as

--------------------------------------

main :: IO ()
main = do
  initializeAll
  window <- getWindow
  renderer <- createRenderer window (-1) defaultRenderer

  --Generation d'un terrain à partir d'un fichier
  terrain <-terrainGenerator "src/test.txt"
  let (Terrain ht lg contenu)= terrain

  -- chargement du sol
  (tmap, smap) <- loadSol renderer "assets/sol.png" TM.createTextureMap SM.createSpriteMap
  --chargement des murs
  (tmap,smap) <- loadMurs renderer "assets/murtest.png"  tmap smap--Ici, il faudra une fonction pour optenir le nombre de mur dans la map
  --chargement des coffres
  (tmap, smap) <- loadCoffre renderer "assets/coffre.png" tmap smap
  --chargement des portes
  (tmap, smap) <- loadPorteOuvert renderer "assets/porteouvert.png" tmap smap
  (tmap, smap) <- loadPorteFerme renderer "assets/porteferme.png" tmap smap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso2.png" tmap smap

  -- initialisation de l'état du jeu

  let gameState = M.initGameState persoX persoY terrain --px et py sont les coordonnées de la map placé sur l'écran
  
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap' smap' kbd gameState

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  clear renderer
  --- display toutes les couches du background
  
  let (Terrain ht lg contenu)= (M.terrain gameState)
  
  displayBackground renderer tmap smap 0 (ht*20) (lg*20) (fromIntegral (M.transX gameState)) (fromIntegral (M.transY gameState)) contenu
  --- display perso 
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 persoX
                                persoY)
  
  
  --M.collision2 gameState
  print (gameState)
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
  

  let gameState'' = M.gameStep gameState kbd' deltaTime
  ---------------------------
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState'')
