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

--Renvoie la fenêtre de l'écran
getWindow:: MonadIO m => m Window
getWindow = createWindow "Dungeon Crawler Universe" $ defaultWindow { windowInitialSize = V2 largeurWin hauteurWin}

--sizeWindows :: Window -> IO (CInt, CInt)
--sizeWindows win= do
  --stateValue <- get (windowSize win)
  --let (V2 x y) = stateValue
  --return (x, y)

loadSol:: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> CInt -> CInt-> IO (TextureMap, SpriteMap) 
loadSol renderer path tmap smap cpt posx posy= do
  tmap' <- TM.loadTexture renderer path (TextureId ("sol"++(show cpt))) tmap
  let sprite1 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("sol"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 16pixel
  let sprite2 = (S.moveTo sprite1 posx posy)
  let smap' = SM.addSprite (SpriteId ("sol"++(show cpt))) sprite2 smap
  if posx < 780 then (loadSol renderer path tmap' smap' (cpt+1) (posx+20) posy)  -- 800 -> la hauteur et 700 la largeur
    else if posy < 680 then (loadSol renderer path tmap' smap' (cpt+1) 0 (posy+20))
      else return (tmap', smap') 

displaySol::Renderer->TextureMap -> SpriteMap -> CInt -> IO ()
displaySol renderer tmap smap cpt = do
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId ("sol"++(show cpt))) smap)
  if cpt+1 == 1400 then return () else (displaySol renderer tmap smap (cpt+1))
  
--Charge les murs du dongeon
loadMurs :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap,Map Coord Case) 
loadMurs renderer tmap smap= do
  (tmap, smap,carte) <- loadMurGauche renderer "assets/mur_gauche_tuile.png" tmap smap 0 0 0 empty
  (tmap, smap,carte) <- loadMurHaut renderer "assets/mur_haut.png" tmap smap 0 0 0 carte
  (tmap, smap,carte) <- loadMurDroit renderer "assets/mur_droit_tuile.png" tmap smap 0 780 0 carte
  (tmap, smap,carte) <- loadMurBas renderer "assets/mur_haut.png" tmap smap 0 0 680 carte
  return (tmap, smap, carte)

loadMurGauche :: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> CInt -> CInt-> Map Coord Case -> IO (TextureMap, SpriteMap,Map Coord Case) 
loadMurGauche renderer path tmap smap cpt posx posy carte= do
  tmap' <- TM.loadTexture renderer path (TextureId ("murG"++(show cpt))) tmap
  let sprite1 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("murG"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 20pixel
  let sprite2 = (S.moveTo sprite1 posx posy)
  let smap' = SM.addSprite (SpriteId ("murG"++(show cpt))) sprite2 smap
  let newcarte = Map.insert (Coord posx posy) Mur carte --On insere les coordonnées du mur dans la map
  if posy <= (700-20) then (loadMurGauche renderer path tmap' smap' (cpt+1) 0 (posy+20) newcarte)  -- 700 -> la largeur
    else return (tmap',smap',carte)
      
displayMurGauche::Renderer->TextureMap -> SpriteMap -> CInt -> IO ()
displayMurGauche renderer tmap smap cpt = do
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId ("murG"++(show cpt))) smap)
  if cpt+1 == 35 then return () else (displayMurGauche renderer tmap smap (cpt+1))

loadMurDroit :: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> Map Coord Case -> IO (TextureMap, SpriteMap,Map Coord Case) 
loadMurDroit renderer path tmap smap cpt posx posy carte= do
  tmap' <- TM.loadTexture renderer path (TextureId ("murD"++(show cpt))) tmap
  let sprite1 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("murD"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 16pixel
  let sprite2 = (S.moveTo sprite1 posx posy)
  let smap' = SM.addSprite (SpriteId ("murD"++(show cpt))) sprite2 smap
  let newcarte = Map.insert (Coord posx posy) Mur carte --On insere les coordonnées du mur dans la map
  if posy <= (700-20) then (loadMurDroit renderer path tmap' smap' (cpt+1) 780 (posy+20) newcarte)  -- 700 -> la largeur
    else return (tmap',smap',carte)
      
displayMurDroit::Renderer->TextureMap -> SpriteMap -> CInt -> IO ()
displayMurDroit renderer tmap smap cpt = do
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId ("murD"++(show cpt))) smap)
  if cpt+1 == 35 then return () else (displayMurDroit renderer tmap smap (cpt+1))

loadMurHaut :: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> CInt -> CInt-> Map Coord Case -> IO (TextureMap, SpriteMap,Map Coord Case) 
loadMurHaut renderer path tmap smap cpt posx posy carte= do
  tmap' <- TM.loadTexture renderer path (TextureId ("murH"++(show cpt))) tmap
  let sprite1 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("murH"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 16pixel
  let sprite2 = (S.moveTo sprite1 posx posy)
  let smap' = SM.addSprite (SpriteId ("murH"++(show cpt))) sprite2 smap
  let newcarte = Map.insert (Coord posx posy) Mur carte --On insere les coordonnées du mur dans la map
  if posx <= 780 then (loadMurHaut renderer path tmap' smap' (cpt+1) (posx+20) 0 newcarte)  -- 700 -> la largeur
    else return (tmap',smap',carte)
      
displayMurHaut::Renderer->TextureMap -> SpriteMap -> CInt -> IO ()
displayMurHaut renderer tmap smap cpt = do
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId ("murH"++(show cpt))) smap)
  if cpt+1 == 40 then return () else (displayMurHaut renderer tmap smap (cpt+1))

loadMurBas :: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> CInt -> CInt-> Map Coord Case -> IO (TextureMap, SpriteMap,Map Coord Case) 
loadMurBas renderer path tmap smap cpt posx posy carte= do
  tmap' <- TM.loadTexture renderer path (TextureId ("murB"++(show cpt))) tmap
  let sprite1 = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("murB"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 16pixel
  let sprite2 = (S.moveTo sprite1 posx posy)
  let smap' = SM.addSprite (SpriteId ("murB"++(show cpt))) sprite2 smap
  let newcarte = Map.insert (Coord posx posy) Mur carte --On insere les coordonnées du mur dans la map
  if posx <= 780 then (loadMurBas renderer path tmap' smap' (cpt+1) (posx+20) 680 newcarte)  -- 700 -> la largeur
    else return (tmap',smap',carte)
      
displayMurBas::Renderer->TextureMap -> SpriteMap -> CInt -> IO ()
displayMurBas renderer tmap smap cpt = do
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId ("murB"++(show cpt))) smap)
  if cpt+1 == 40 then return () else (displayMurBas renderer tmap smap (cpt+1))

--Chargement animation perso
loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 25 45)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

--A la creation je place mes blocs de sol----------------------------------
displayBackground:: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> IO ()
displayBackground renderer tmap smap cpt posx posy = do
  displaySol renderer tmap smap 0 --display le sol
  displayMur renderer tmap smap 0 0 0--display murs
  --display portes


displayMur :: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> IO ()
displayMur renderer tmap smap cpt posx posy = do
    displayMurHaut renderer tmap smap cpt 
    displayMurGauche renderer tmap smap cpt
    displayMurDroit renderer tmap smap cpt
    displayMurBas renderer tmap smap cpt 


--------------------------------------


main :: IO ()
main = do
  initializeAll
  window <- getWindow
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadSol renderer "assets/sol.png" TM.createTextureMap SM.createSpriteMap 0 0 0
  --chargement des mur (Autour de la fenêtre)
  (tmap, smap, carte) <- loadMurs renderer tmap smap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.png" tmap smap

  -- initialisation de l'état du jeu
  let terrain = initTerrain 35 40 carte
  print (carte)
  --Position du jouer (Random pour l'instant)
  vx <- randomRIO (50,526) :: IO Int
  vy <- randomRIO (50,292) :: IO Int

  --Initialisation de l'etat du jeu
  let gameState = M.initGameState vx vy (fromIntegral largeurWin) (fromIntegral hauteurWin)
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
  displayBackground renderer tmap smap 110 0 0
  --- display perso 
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 (fromIntegral (M.persoX gameState))
                                 (fromIntegral (M.persoY gameState)))

 

  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState'' = M.gameStep gameState kbd' deltaTime

  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState'')
