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

--Charge le sol du donjon (tous les blocs  à la position 0 0)
loadSol:: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> IO (TextureMap, SpriteMap) 
loadSol rdr path tmap smap cpt = do
  tmap' <- TM.loadTexture rdr path (TextureId ("sol"++(show cpt))) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("sol"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 16pixel
  let smap' = SM.addSprite (SpriteId ("sol"++(show cpt))) sprite smap
  if cpt <= 1400 then (loadSol rdr path tmap' smap' (cpt+1)) else return (tmap', smap') -- 1400 -> (hauteur/tailleBloc) * (largeur /tailleBloc)

loadMurGauche:: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> IO (TextureMap, SpriteMap) 
loadMurGauche rdr path tmap smap cpt = do
  tmap' <- TM.loadTexture rdr path (TextureId ("murG"++(show cpt))) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("murG"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 16pixel
  let smap' = SM.addSprite (SpriteId ("murG"++(show cpt))) sprite smap
  if cpt <= 35 then (loadMurGauche rdr path tmap' smap' (cpt+1)) else return (tmap', smap') -- 35 -> (hauteur/tailleBloc) 

loadMurDroit:: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> IO (TextureMap, SpriteMap) 
loadMurDroit rdr path tmap smap cpt = do
  tmap' <- TM.loadTexture rdr path (TextureId ("murD"++(show cpt))) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("murD"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 16pixel
  let smap' = SM.addSprite (SpriteId ("murD"++(show cpt))) sprite smap
  if cpt <= 35 then (loadMurDroit rdr path tmap' smap' (cpt+1)) else return (tmap', smap') -- 35 -> (hauteur/tailleBloc) 

loadMurHaut:: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> IO (TextureMap, SpriteMap) 
loadMurHaut rdr path tmap smap cpt = do
  tmap' <- TM.loadTexture rdr path (TextureId ("murH"++(show cpt))) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("murH"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 16pixel
  let smap' = SM.addSprite (SpriteId ("murH"++(show cpt))) sprite smap
  if cpt <= 40 then (loadMurHaut rdr path tmap' smap' (cpt+1)) else return (tmap', smap') -- 40 ->(largeur /tailleBloc)

loadMurBas:: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> IO (TextureMap, SpriteMap) 
loadMurBas rdr path tmap smap cpt = do
  tmap' <- TM.loadTexture rdr path (TextureId ("murB"++(show cpt))) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("murB"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 16pixel
  let smap' = SM.addSprite (SpriteId ("murB"++(show cpt))) sprite smap
  if cpt <= 40 then (loadMurBas rdr path tmap' smap' (cpt+1)) else return (tmap', smap') -- 40 ->(largeur /tailleBloc)

--A la creation je place mes blocs de sol----------------------------------
displayBackground:: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> Map Coord Case -> IO (Map Coord Case)
displayBackground renderer tmap smap cpt transx trany carte= do
  carte <- displaySol renderer tmap smap 0 0 0 transx trany carte--display le sol
  carte <- displayMur renderer tmap smap 0 0 0 transx trany carte--display murs
  return carte
  --display portes


displaySol :: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> CInt -> Map Coord Case -> IO (Map Coord Case)
displaySol renderer tmap smap cpt posx posy transx transy carte= do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("sol"++(show cpt))) smap) (posx+transx) (posy+transy))
  let newcarte = Map.insert (Coord (posx+transx) (posy+transy)) Mur carte --On insere les coordonnées du mur dans la map
  if posx < 780 then (displaySol renderer tmap smap cpt (posx+20) posy transx transy newcarte)  -- 800 -> la hauteur et 700 la largeur
    else if posy < 680 then (displaySol renderer tmap smap cpt 0 (posy+20) transx transy newcarte)
      else return (carte) 

displayMur :: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> CInt -> Map Coord Case -> IO (Map Coord Case)
displayMur renderer tmap smap cpt posx posy transx transy carte= do
     carte <- displayMurHaut renderer tmap smap cpt posx posy transx transy carte
     carte <- displayMurGauche renderer tmap smap cpt posx posy transx transy carte
     carte <- displayMurDroit renderer tmap smap cpt (800-20) posy transx transy carte
     carte <- displayMurBas renderer tmap smap cpt posx (700-20) transx transy carte
     return carte

displayMurDroit::Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> CInt -> Map Coord Case -> IO (Map Coord Case)
displayMurDroit renderer tmap smap cpt posx posy transx transy carte= do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("murD"++(show cpt))) smap) (posx+transx) (posy+transy))
  let newcarte = Map.insert (Coord (posx+transx) (posy+transy)) Mur carte --On insere les coordonnées du mur dans la map
  if posy < (700-20) then (displayMurDroit renderer tmap smap cpt (800-20) (posy+20) transx transy newcarte)  -- 800 -> la dernière case (en haut a droite )
    else return (carte)


displayMurGauche::Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> CInt -> Map Coord Case -> IO (Map Coord Case)
displayMurGauche renderer tmap smap cpt posx posy transx transy carte = do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("murG"++(show cpt))) smap) (posx+transx) (posy+transy))
  let newcarte = Map.insert (Coord (posx+transx) (posy+transy)) Mur carte --On insere les coordonnées du mur dans la map
  if posy < (700-20) then (displayMurGauche renderer tmap smap cpt 0 (posy+20) transx transy newcarte)  -- 700 -> la largeur
    else return (carte)

displayMurHaut::Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> CInt -> Map Coord Case -> IO (Map Coord Case)
displayMurHaut renderer tmap smap cpt posx posy transx transy carte= do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("murH"++(show cpt))) smap) (posx+transx) (posy+transy))
  let newcarte = Map.insert (Coord (posx+transx) (posy+transy)) Mur carte --On insere les coordonnées du mur dans la map
  if posx < (800-20) then (displayMurHaut renderer tmap smap cpt (posx+20) 0 transx transy newcarte)  -- 700 -> la largeur
    else return (carte)

displayMurBas::Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> CInt -> CInt -> Map Coord Case -> IO (Map Coord Case)
displayMurBas renderer tmap smap cpt posx posy transx transy carte= do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("murB"++(show cpt))) smap) (posx+transx) (posy+transy))
  let newcarte = Map.insert (Coord (posx+transx) (posy+transy)) Mur carte --On insere les coordonnées du mur dans la map
  if posx < (800-20) then (displayMurBas renderer tmap smap cpt (posx+20) (700-20) transx transy newcarte)  -- 700 -> la largeur
    else return (carte)

--------------------------------------

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 25 45)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')


main :: IO ()
main = do
  initializeAll
  window <- getWindow
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadSol renderer "assets/sol.png" TM.createTextureMap SM.createSpriteMap 0
  --chargement des mur (Autour de la fenêtre)
  (tmap, smap) <- loadMurGauche renderer "assets/mur_gauche_tuile.png" tmap smap 0
  (tmap, smap) <- loadMurHaut renderer "assets/mur_haut.png" tmap smap 0
  (tmap, smap) <- loadMurDroit renderer "assets/mur_droit_tuile.png" tmap smap 0
  (tmap, smap) <- loadMurBas renderer "assets/mur_haut.png" tmap smap 0
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.png" tmap smap

  -- initialisation de l'état du jeu
  --taille de la fenetre du client
  --(wx,wy) <- (sizeWindows window)
  let gameState = M.initGameState 0 0 1000 1000
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
 
  carte <- displayBackground renderer tmap smap 0 (fromIntegral (M.persoX gameState)) (fromIntegral (M.persoY gameState)) empty
  --print (carte)
  --- display perso 
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 380
                                 320)

  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state----
  let terrain= initTerrain 750 800 carte -- A ajouter à l'état du jeu 
  let gameState'' = M.gameStep gameState kbd' deltaTime
  ---------------------------
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState'')
