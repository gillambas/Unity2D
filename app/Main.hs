module Main where

import qualified Apecs              as A
import qualified Apecs.Gloss        as AG
import qualified Linear             as L

import qualified Components         as C
import qualified EventHandling      as EH
import qualified Step               as Step
import qualified Systems.Initialise as SInit
import qualified Visualise.Draw     as Draw
import qualified Visualise.Load     as Load
import qualified Visualise.Tools    as VTools


main :: IO ()
main = do
  picBundle <- Load.createPictureBundle

  w <- C.initWorld

  A.runWith w $ do
    SInit.initialise config picBundle
    boardPic <- Draw.createBoardPicture config picBundle
    AG.play 
      AG.FullScreen 
      AG.white 
      60 
      (Draw.draw boardPic config picBundle) 
      EH.eventHandler 
      Step.stepper


config = C.Config
  { C.bottomLeft         = (-1, -1)
  , C.topRight           = (8, 8)
  , C.exitPosition       = C.CPosition $ L.V2 7 7 
  , C.startPosition      = C.CPosition $ L.V2 0 0
  , C.foodPointsPosition = C.CPosition $ L.V2 2 (-1)

  , C.foodPoints         = 100
  , C.foodRange          = (1, 5)
  , C.innerWallsRange    = (5, 9)
  , C.innerWallHealth    = C.CHealth 4 1
  , C.nutrition          = C.Nutrition {C.soda = 20, C.fruit = 10}
  
  , C.cellWidth          = VTools.spriteWidth 
  , C.cellHeight         = VTools.spriteHeight
  }