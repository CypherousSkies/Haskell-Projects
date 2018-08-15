module Dungeon
        (Style(Mixed,Death_Trap,Lair,Planar_Gate,Maze,Mine,Stronghold,Temple,Tomb,Treasure_Vault)
	,DNode(Chamber,Door,Stairs,Passage)
        ,NDoor(NormalDoor,SecretDoor)
        ,Hazard
        ,genDungeon
        ) where

import Random.Dist
import Random.Tree
import System.Random

data Style = Mixed | Death_Trap | Lair | Planar_Gate | Maze | Mine | Stronghold | Temple | Tomb | Treasure_Vault

data DNode = Chamber Int Int Hazard String | Door NDoor | Stairs Int | Passage Int String

data NDoor = NormalDoor {locked::Bool, material::String} | SecretDoor

costs::[(DNode,Double)]

hazard::DNode -> Double

nodeDist::RDist DNode

genDungeon::RandomGen g => Style -> g -> Map DNode
