module Gametest where

import Test.Hspec
import Test.QuickCheck

import Carte

import Data.Map


-- Test avec 'Should be'
mapTest:: (Map Coord Case)
mapTest = fromList [((Coord 4 1), Mur), ((Coord 3 2), Zombie),((Coord 4 2),Mur),((Coord 3 7), Sortie), ((Coord 1 1), Entree)]

mapTest2:: (Map Coord Case)
mapTest2 = fromList [((Coord 4 1), Mur), ((Coord 3 2), Zombie),((Coord 3 1), Sortie),((Coord 3 7), Sortie), ((Coord 1 1), Entree), ((Coord 8 8), Entree)]

--Verifie qu'une case est vide (vide == Nothing)
getCaseFromString_spec0 = do
    describe "getCaseFromString0" $ do
        it "Recupère une case à partir d'un string (Le string appartient à une case existante)" $ 
            (getCaseFromString "Mur") `shouldBe` (Mur)

getCaseFromString_spec1 = do
    describe "getCaseFromString1" $ do
        it "Recupère une case à partir d'un string (Le string n'appartient à aucune case)" $ 
            (getCaseFromString "Janvier") `shouldBe` (Vide)

--verifie qu'une clé d'un objket dans une map à bien été mise à jour
updateKeyMap_spec0 = do
    describe "updateKeyMap" $ do
        it "Met à jour la clé d'un objet dans une map" $
            (updateKeyMap (Coord 3 2) (Coord 3 3) mapTest) `shouldBe` (fromList [((Coord 4 1), Mur), ((Coord 3 3), Zombie),((Coord 4 2),Mur),((Coord 3 7), Sortie), ((Coord 1 1), Entree)])

--Verifie que la valeur d'un objet dans une map à été mise à jour sinon rajoute dans la map
updateValueMap_spec0 = do
    describe "updateValueMap0" $ do
        it "Met à jour la valeur d'un objet dans une map (l'objet existe dans la map)" $
            (updateValueMap mapTest (Coord 3 2) (Porte EO Ferme)) `shouldBe` (fromList [ ((Coord 4 1), Mur), ((Coord 3 2), (Porte EO Ferme)),((Coord 4 2),Mur),((Coord 3 7), Sortie), ((Coord 1 1), Entree)] )

updateValueMap_spec1 = do
    describe "updateValueMap1" $ do
        it "Met à jour la valeur d'un objet dans une map (l'objet existe pas dans la map)" $
            (updateValueMap mapTest (Coord 5 3) (Porte EO Ferme)) `shouldBe` (fromList [((Coord 4 1), Mur), ((Coord 3 2), Zombie), ((Coord 5 3), (Porte EO Ferme)),((Coord 4 2),Mur),((Coord 3 7), Sortie), ((Coord 1 1), Entree)])

--Verifie si une entite existe dans une map
--testMap_spec = undefined

--Verifie qu'on récupère bien toutes les coordonnées d'une entité dans une map
getCoordonneesObjectMap_spec0 = do
    describe "getCoordonneesObjectMap0" $ do
        it "Recupère les coordonnées d'une entité dans une map (L'entité existe dans la map)" $
            (getCoordonneesObjectMap mapTest (Just Mur)) `shouldBe` ([(Coord 4 1),(Coord 4 2)])

getCoordonneesObjectMap_spec1 = do
    describe "getCoordonneesObjectMap1" $ do
        it "Recupère les coordonnées d'une entité dans une map (L'entité n'existe pas dans la map)" $
            (getCoordonneesObjectMap mapTest (Just (Porte EO Ferme))) `shouldBe` ([])

--Verification d'une collision

collision_spec0 = do
    describe "collision0" $ do
        it "Verifie s'il existe une collision pour une coordonnées donnée (On a une collision)" $
            (collision mapTest 4 1) `shouldBe` (True)

collision_spec1 = do
    describe "collision1" $ do
        it "Verifie s'il existe une collision pour une coordonnées donnée (Pas de collision)" $
            (collision mapTest 7 1) `shouldBe` (False)

-- Verifie qu'il existe une entree et une sortie
getSortie_spec0 = do
    describe "getSortie0" $ do
        it "Verifie qu'il existe une seul sortie dans une map (Il existe une seule sortie)" $
            (getSortie mapTest) `shouldBe` (Coord 3 7)

getEntree_spec0 = do
    describe "getEntree0" $ do 
        it "Verifie qu'il existe une seul entree dans une map (Il existe une seule entree)" $
            (getEntree mapTest) `shouldBe` (Coord 1 1)

getSortie_spec1 = do
    describe "getSortie1" $ do
        it "Verifie qu'il existe une seul sortie dans une map (Il existe deux sortie)" $
            (getSortie mapTest2) `shouldBe` (Coord (-1) (-1))

getEntree_spec1 = do
    describe "getEntree0" $ do 
        it "Verifie qu'il existe une seul entree dans une map (Il existe deux entree)" $
            (getEntree mapTest2) `shouldBe` (Coord (-1) (-1))



testShouldBe = do
    getCaseFromString_spec0
    getCaseFromString_spec1
    updateKeyMap_spec0
    --updateKeyMap_spec1
    updateValueMap_spec0
    updateValueMap_spec1
    getCoordonneesObjectMap_spec0
    getCoordonneesObjectMap_spec1
    collision_spec0
    collision_spec1
    getSortie_spec0
    getEntree_spec0
    getSortie_spec1
    getEntree_spec1