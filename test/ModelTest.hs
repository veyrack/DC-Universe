module ModelTest where

import Test.Hspec
import Test.QuickCheck

import Carte
import Model
import Data.Map

gstate :: GameState
gstate = initGameState (Translation 0 0) (Perso 350 350 North 90) (Terrain 0 0 empty)

changePv_spec0 = do
    describe "test 0 : changePv" $ do
        it "Met à jour les points de vie de son personnage (+10pv)" $
            (changePv gstate 10) `shouldBe` gstate {perso = (Perso 350 350 North 100)}

changePv_spec1 = do
    describe "test 1 : changePv" $ do
        it "Met à jour les points de vie de son personnage (-10pv)" $
            (changePv gstate (-10)) `shouldBe` gstate {perso = (Perso 350 350 North 80)}

testShouldBe = do
    changePv_spec0
    changePv_spec1