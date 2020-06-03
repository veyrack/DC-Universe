module ModelTest where

import Test.Hspec
import Test.QuickCheck

import Carte
import Model
import Data.Map

terr :: Terrain
terr = initTerrain 100 100 (fromList [(Coord 10 10,Pique Ferme),
                                        (Coord 20 20,Porte NS Ferme),
                                        (Coord 30 30,Coffre Ferme),
                                        (Coord 18 18,Sortie)
                                        ])

terr2 :: Terrain
terr2 = initTerrain 100 100 (fromList [(Coord 10 10,Porte NS Ouvert)])

gstate :: GameState
gstate = initGameState (Translation 0 0) (Perso 350 350 North 90 empty) terr --(Terrain 0 0 empty)

gstate2 :: GameState
gstate2 = initGameState (Translation 0 0) (Perso 350 350 North 90 empty) terr2 --(Terrain 0 0 empty)

gstate3 :: GameState
gstate3 = initGameState (Translation 0 0) (Perso 350 350 North 90 (fromList [(Potion,0),(Masque,0)])) terr --(Terrain 0 0 empty)

gstate4 :: GameState
gstate4 = initGameState (Translation 0 0) (Perso 350 350 North 90 (fromList [(Potion,0),(Masque,1)])) terr

changePv_spec0 = do
    describe "test 0 : changePv" $ do
        it "Met à jour les points de vie de son personnage (+10pv)" $
            (changePv gstate 10) `shouldBe` gstate {perso = (Perso 350 350 North 100 empty)}

changePv_spec1 = do
    describe "test 1 : changePv" $ do
        it "Met à jour les points de vie de son personnage (-10pv)" $
            (changePv gstate (-10)) `shouldBe` gstate {perso = (Perso 350 350 North 80 empty)}

moveLeft_spec = do
    describe "test : move left" $
        it "Deplace le personnage une fois a gauche" $
            (moveLeft gstate) `shouldBe` gstate {translate = (Translation 4 0), perso = (Perso 350 350 West 90 empty)}

moveRight_spec = do
    describe "test : move right" $
        it "Deplace le personnage une fois a droite" $
            (moveRight gstate) `shouldBe` gstate {translate = (Translation (-4) 0), perso = (Perso 350 350 East 90 empty)}

moveUp_spec = do
    describe "test : move up" $
        it "Deplace le personnage une fois en haut" $
            (moveUp gstate) `shouldBe` gstate {translate = (Translation 0 4), perso = (Perso 350 350 North 90 empty)}

moveDown_spec = do
    describe "test : move down" $
        it "Deplace le personnage une fois en bas" $
            (moveDown gstate) `shouldBe` gstate {translate = (Translation 0 (-4)), perso = (Perso 350 350 South 90 empty)}

openEntity_spec = do
    describe "test : open entity" $
        it "Ouvre un pique initialement ferme" $
            (openEntity gstate "Pique Ferme" 10 10) `shouldBe` gstate {terrain = initTerrain 100 100 (fromList [(Coord 10 10,Pique Ouvert),
                                                                                                            (Coord 20 20,Porte NS Ferme),
                                                                                                            (Coord 30 30,Coffre Ferme),
                                                                                                            (Coord 18 18,Sortie)
                                                                                                            ])}

openDoor_spec = do
    describe "test : open door" $
        it "Ouvre une porte initialement ferme" $
            (openaDoor gstate 20 20) `shouldBe` gstate {terrain = initTerrain 100 100 (fromList [(Coord 10 10,Pique Ferme),
                                                                                                (Coord 20 20,Porte NS Ouvert),
                                                                                                (Coord 30 30,Coffre Ferme),
                                                                                                (Coord 18 18,Sortie)
                                                                                                ])}
closeDoor_spec = do
    describe "test : close door" $
        it "Ferme une porte initialement ouverte" $
            (openaDoor gstate2 10 10) `shouldBe` gstate2 {terrain = initTerrain 100 100 (fromList [(Coord 10 10,Porte NS Ferme)])}
                                                                                                        
openChest_spec = do
    describe "test : open chest" $
        it "Ouvre un coffre initialement ferme et donne une potion de vie ou un masque" $
            (openChest gstate3 "Coffre Ferme" 30 30) `shouldBe` gstate3 {perso = (Perso 350 350 North 90 (fromList [(Potion,1),(Masque,0)])),
                                                                                terrain = (initTerrain 100 100 (fromList [(Coord 10 10,Pique Ferme),
                                                                                                (Coord 20 20,Porte NS Ferme),
                                                                                                (Coord 30 30,Coffre Ouvert),
                                                                                                (Coord 18 18,Sortie)]))}

testSortie_spec = do
    describe "test de sortie du jeu" $ 
        it "Check si le personnage peut accéder à la sortie (Il faut le trésor)" $ 
            (testSortie gstate4) `shouldBe` True

testPiege_spec = do
    describe "test piege" $
        it "Check si un piege s'active correctement en repoussant le perso et en enlevant de la vie" $
            (actionPiqueFerme gstate) `shouldBe` gstate {translate = (Translation 0 (-8)),
                                                        perso = (Perso 350 350 North 80 empty)}


testShouldBe = do
    changePv_spec0
    changePv_spec1
    moveLeft_spec
    moveRight_spec
    moveUp_spec
    moveDown_spec
    openEntity_spec
    openDoor_spec
    closeDoor_spec
    openChest_spec
    testSortie_spec
    testPiege_spec