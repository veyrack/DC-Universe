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
      


--if posx <= 800 then (loadSol renderer tmap smap (cpt+1) (posx+20) posy)  -- 800 -> la hauteur et 700 la largeur
    --else if (posx>= 800 && posy>=700) then return () else (loadSol renderer tmap smap (cpt+1) 0 (posy+20))

displaySol::Renderer->TextureMap -> SpriteMap -> CInt -> IO ()
displaySol renderer tmap smap cpt = do
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId ("sol"++(show cpt))) smap)
  if cpt+1 == 1400 then return () else (displaySol renderer tmap smap (cpt+1))
{--
--Charge le sol du donjon (tous les blocs  à la position 0 0)
loadSol:: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> IO (TextureMap, SpriteMap) 
loadSol rdr path tmap smap cpt = do
  tmap' <- TM.loadTexture rdr path (TextureId ("sol"++(show cpt))) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("sol"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 16pixel
  let smap' = SM.addSprite (SpriteId ("sol"++(show cpt))) sprite smap
  if cpt <= 1400 then (loadSol rdr path tmap' smap' (cpt+1)) else return (tmap', smap') -- 1400 -> (hauteur/tailleBloc) * (largeur /tailleBloc)
--}

--Charge les murs du dongeon
loadMurs :: Renderer -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap) 
loadMurs renderer tmap smap= do
  (tmap, smap) <- loadMurGauche renderer "assets/mur_gauche_tuile.png" tmap smap 0
  (tmap, smap) <- loadMurHaut renderer "assets/mur_haut.png" tmap smap 0
  (tmap, smap) <- loadMurDroit renderer "assets/mur_droit_tuile.png" tmap smap 0
  (tmap, smap) <- loadMurBas renderer "assets/mur_haut.png" tmap smap 0
  return (tmap, smap)

loadMurGauche:: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> IO (TextureMap, SpriteMap) 
loadMurGauche rdr path tmap smap cpt = do
  tmap' <- TM.loadTexture rdr path (TextureId ("murG"++(show cpt))) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("murG"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 20pixel
  let smap' = SM.addSprite (SpriteId ("murG"++(show cpt))) sprite smap
  if cpt <= 35 then (loadMurGauche rdr path tmap' smap' (cpt+1)) else return (tmap', smap') -- 35 -> (hauteur/tailleBloc) 

loadMurDroit:: Renderer-> FilePath -> TextureMap -> SpriteMap -> CInt -> IO (TextureMap, SpriteMap) 
loadMurDroit rdr path tmap smap cpt = do
  tmap' <- TM.loadTexture rdr path (TextureId ("murD"++(show cpt))) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId ("murD"++(show cpt))) (S.mkArea 0 0 20 20) --bloc de 20pixel
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
  --displayMur renderer tmap smap 0 0 0--display murs
  --display portes

{--
displaySol :: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> IO ()
displaySol renderer tmap smap cpt posx posy = do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("sol"++(show cpt))) smap) posx posy)
  if posx <= 800 then (displaySol renderer tmap smap cpt (posx+20) posy)  -- 800 -> la hauteur et 700 la largeur
    else if (posx>= 800 && posy>=700) then return () else (displaySol renderer tmap smap cpt 0 (posy+20))
--}

displayMur :: Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> IO ()
displayMur renderer tmap smap cpt posx posy = do
    displayMurHaut renderer tmap smap cpt posx posy
    displayMurGauche renderer tmap smap cpt posx posy
    displayMurDroit renderer tmap smap cpt (800-20) posy
    displayMurBas renderer tmap smap cpt posx (700-20)

displayMurDroit::Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> IO ()
displayMurDroit renderer tmap smap cpt posx posy = do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("murD"++(show cpt))) smap) posx posy)
  if posy <= (700-20) then (displayMurDroit renderer tmap smap cpt (800-20) (posy+20))  -- 800 -> la dernière case (en haut a droite )
    else return ()
    

displayMurGauche::Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> IO ()
displayMurGauche renderer tmap smap cpt posx posy = do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("murG"++(show cpt))) smap) posx posy)
  if posy <= (700-20) then (displayMurGauche renderer tmap smap cpt 0 (posy+20))  -- 700 -> la largeur
    else return ()

displayMurHaut::Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> IO ()
displayMurHaut renderer tmap smap cpt posx posy = do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("murH"++(show cpt))) smap) posx posy)
  if posx <= (800-20) then (displayMurHaut renderer tmap smap cpt (posx+20) 0)  -- 700 -> la largeur
    else return ()

displayMurBas::Renderer->TextureMap -> SpriteMap -> CInt -> CInt -> CInt -> IO ()
displayMurBas renderer tmap smap cpt posx posy = do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId ("murB"++(show cpt))) smap) posx posy)
  if posx <= (800-20) then (displayMurBas renderer tmap smap cpt (posx+20) (700-20))  -- 700 -> la largeur
    else return ()

--------------------------------------


main :: IO ()
main = do
  initializeAll
  window <- getWindow
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadSol renderer "assets/sol.png" TM.createTextureMap SM.createSpriteMap 0 0 0
  --chargement des mur (Autour de la fenêtre)
  --(tmap, smap) <- loadMurs renderer tmap smap
 
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.png" tmap smap

  -- initialisation de l'état du jeu
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