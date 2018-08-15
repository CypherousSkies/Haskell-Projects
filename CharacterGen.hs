module CharacterGen
	(genCharacter
	,genNChars
	) where

import Random.Dist
import System.Random

data Races = Arakocra 


-- STR DEX CON INT WIS CHA
racialBonus::Races -> [Int]    --[S,D,C,I,W,C]
racialBonus Arakocra           = [0,2,0,0,1,0]
racialBonus Aasimar_Protector  = [0,0,0,0,1,2]
racialBonus Aasimar_Scourge    = [0,0,1,0,0,2]
racialBonus Aasimar_Fallen     = [1,0,0,0,0,2]
racialBonus Bugbear            = [2,1,0,0,0,0]
racialBonus Centaur            = [2,0,0,0,1,0]
racialBonus Changling_INT      = [0,0,0,1,0,2]
racialBonus Changling_DEX      = [0,1,0,0,0,2]
racialBonus Dragonborn         = [2,0,0,0,0,1]
racialBonus Dwarf_Hill         = [0,0,2,0,1,0]
racialBonus Dwarf_Mountain     = [2,0,2,0,0,0]
racialBonus Dwarf_Gray         = [1,0,2,0,0,0]
racialBonus Dwarf_Warding      = [0,0,2,0,0,0]
racialBonus Tiefling_Feral     = [0,2,0,1,0,0]
racialBonus Firbolg            = [1,0,0,0,2,0]
racialBonus Genasi_Air         = [0,1,2,0,0,0]
racialBonus Genasi_Earth       = [1,0,2,0,0,0]
racialBonus Genasi_Fire        = [0,0,2,1,0,0]
racialBonus Genasi_Water       = [0,0,2,0,1,0]
racialBonus Githyanki          = [2,0,0,1,0,0]
racialBonus Githzerai          = [0,0,0,1,2,0]
racialBonus Gnome_Rock         = [0,0,0,2,0,0]
racialBonus Gnome_Deep         = [0,0,0,2,0,0]
racialBonus Gnome_Forest       = [0,0,0,2,0,0]
racialBonus Gnome_Rock         = [0,0,0,2,0,0]



race::Dist Race

background

