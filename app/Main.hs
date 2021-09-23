module Main where

import qualified Apecs              as A
import qualified Apecs.Gloss        as AG

import qualified Components         as C
import qualified EventHandling      as EH
import qualified Step               as Step
import qualified Systems.Initialise as SInit
import qualified Visualise.Draw     as Draw
import qualified Visualise.Load     as Load


main :: IO ()
main = do
  picBundle <- Load.createPictureBundle

  w <- C.initWorld
  
  A.runWith w $ do
    A.set A.global picBundle
    SInit.startNewGame

    AG.play 
      AG.FullScreen
      AG.black
      60
      Draw.draw
      EH.eventHandler
      Step.stepper