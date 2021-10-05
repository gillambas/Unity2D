module Main where

import qualified Apecs                as A
import qualified Apecs.Gloss          as AG

import qualified Components           as C
import qualified HandleInput.Keyboard as HIK
import qualified Step                 as Step
import qualified Systems.Initialise   as SInit
import qualified Visualise.Draw       as Draw
import qualified Visualise.Load       as Load


main :: IO ()
main = do
  graphics <- Load.loadGraphics

  w <- C.initWorld
  
  A.runWith w $ do
    A.set A.global graphics
    SInit.startNewGame

    AG.play 
      AG.FullScreen
      AG.black
      60
      Draw.draw
      HIK.eventHandler
      Step.stepper