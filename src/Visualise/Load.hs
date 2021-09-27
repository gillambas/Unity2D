-- | Functions for loading any images used in the game.
-- N.B.: The top left sprite in the sheet has coordinate (0,0).

module Visualise.Load (
  loadGraphics
)
where

import qualified Apecs.Gloss     as AG
import qualified Codec.Picture   as CP
import qualified Data.Map        as Map

import qualified Components      as C
import qualified Visualise.Tools as VTools


----------------------------------------------------------------------------------------------
-----------------------                GLOBAL VARIABLES                -----------------------
----------------------------------------------------------------------------------------------
spriteSheetPath :: FilePath
spriteSheetPath = "assets/sprites/Scavengers_SpriteSheet.png"

fontPath :: FilePath 
fontPath = "assets/fonts/PressStart2P-Regular.ttf"
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                   LOAD IMAGES                  -----------------------
----------------------------------------------------------------------------------------------
loadGraphics :: IO C.CGraphics
loadGraphics = do 
  font        <- VTools.loadFontFile' fontPath
  spriteSheet <- loadSpriteSheet

  let damagedWalls  = loadInnerWallsDamaged spriteSheet
      exit          = loadExit spriteSheet
      floor         = loadFloorTiles spriteSheet
      fruit         = loadFruit spriteSheet
      intactWalls   = loadInnerWallsIntact spriteSheet
      outerWalls    = loadOuterWallTiles spriteSheet
      soda          = loadSoda spriteSheet
      playerAttack  = loadPlayerAttack spriteSheet
      playerHurt    = loadPlayerHurt spriteSheet
      playerIdle    = loadPlayerIdle spriteSheet
      vampireAttack = loadVampireAttack spriteSheet
      vampireIdle   = loadVampireIdle spriteSheet
      zombieAttack  = loadZombieAttack spriteSheet
      zombieIdle    = loadZombieIdle spriteSheet

  let damagedWalls' = Map.fromList $ zip [minBound .. maxBound] damagedWalls
      floor'        = Map.fromList $ zip [minBound .. maxBound] floor
      outerWalls'   = Map.fromList $ zip [minBound .. maxBound] outerWalls
      intactWalls'  = Map.fromList $ zip [minBound .. maxBound] intactWalls

  let graphics = C.CGraphics
        { C.damagedInnerWallPics = damagedWalls'
        , C.exitPic              = exit
        , C.floorPics            = floor'
        , C.font                 = font
        , C.fruitPic             = fruit
        , C.intactInnerWallPics  = intactWalls'
        , C.outerWallPics        = outerWalls'
        , C.playerAttackPics     = playerAttack
        , C.playerHurtPics       = playerHurt  
        , C.playerIdlePics       = playerIdle
        , C.sodaPic              = soda
        , C.vampireAttackPics    = vampireAttack
        , C.vampireIdlePics      = vampireIdle 
        , C.zombieAttackPics     = zombieAttack
        , C.zombieIdlePics       = zombieIdle
        }

  return graphics


loadSpriteSheet :: IO CP.DynamicImage
loadSpriteSheet = VTools.readImage' spriteSheetPath


loadExit  = loadPic' (4,2)


loadFloorTiles = loadPics'
  [ (0, 4)
  , (1, 4)
  , (2, 4)
  , (3, 4)
  , (4, 4)
  , (5, 4)
  , (6, 4)
  , (7, 4) ]


loadFruit = loadPic' (3,2)


loadInnerWallsDamaged = loadPics'
  [ (0, 6)
  , (1, 6)
  , (2, 6)
  , (3, 6)
  , (4, 6)
  , (5, 6) 
  , (6, 6) ]

loadInnerWallsIntact = loadPics'
  [ (5, 2)
  , (6, 2)
  , (7, 2)
  , (0, 3)
  , (3, 3)
  , (6, 3) 
  , (7, 3) ]


loadOuterWallTiles = loadPics' 
  [ (1, 3)
  , (2, 3)
  , (4, 3) ]


loadPlayerAttack = loadPics' 
  [ (0, 5)
  , (1, 5) ]

loadPlayerHurt = loadPics' 
  [ (6, 5)
  , (7, 5) ]

loadPlayerIdle = loadPics' 
  [ (0, 0)
  , (1, 0)
  , (2, 0)
  , (3, 0)
  , (4, 0)
  , (5, 0) ]


loadSoda  = loadPic' (2,2)


loadVampireAttack = loadPics' 
  [ (4, 5)
  , (5, 5) ]
  
loadVampireIdle = loadPics' 
  [ (4, 1)
  , (5, 1)
  , (6, 1)
  , (7, 1)
  , (0, 2)
  , (1, 2) ]


loadZombieAttack = loadPics' 
  [ (2, 5)
  , (3, 5) ]

loadZombieIdle = loadPics'
  [ (6, 0)
  , (7, 0)
  , (0, 1)
  , (1, 1)
  , (2, 1)
  , (3, 1) ]
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                GENERAL FUNCTIONS               -----------------------
----------------------------------------------------------------------------------------------
loadPic' :: (Int, Int) -> CP.DynamicImage -> AG.Picture 
loadPic' = loadPic C.spriteWidth C.spriteHeight


loadPic :: Int -> Int -> (Int, Int) -> CP.DynamicImage -> AG.Picture 
loadPic cellWidth cellHeight (row, col) img = croppedPic 
  where
    x = row * cellWidth
    y = col * cellHeight
    croppedImg = VTools.cropDynamicImage x y cellWidth cellHeight img
    croppedPic = VTools.fromDynamicImage' croppedImg


loadPics' :: [(Int, Int)] -> CP.DynamicImage -> [AG.Picture]
loadPics' = loadPics C.spriteWidth C.spriteHeight


loadPics :: Int -> Int -> [(Int, Int)] -> CP.DynamicImage -> [AG.Picture]
loadPics cellWidth cellHeight rowsCols img = map (\rc -> loadPic cellWidth cellHeight rc img) rowsCols
----------------------------------------------------------------------------------------------