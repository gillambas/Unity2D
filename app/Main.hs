module Main where

import qualified Apecs                  as A
import qualified Apecs.Gloss            as AG
import qualified Device.Nintendo.Switch as NS

import qualified Components             as C
import qualified HandleInput.Keyboard   as HIK
import qualified HandleInput.Switch     as Switch
import qualified Step                   as Step
import qualified Systems.Initialise     as SInit
import qualified Visualise.Draw         as Draw
import qualified Visualise.Load         as Load


main :: IO ()
main = do
  graphics <- Load.loadGraphics

  switch <- NS.init
  switchControllers <- Switch.loadSwitchControllers switch

  w <- C.initWorld
  
  A.runWith w $ do
    A.set A.global graphics
    A.set A.global switchControllers

    SInit.startNewGame

    AG.play 
      AG.FullScreen
      AG.black
      60
      Draw.draw
      HIK.eventHandler
      Step.stepper

    C.CSwitchControllers left right pro <- A.get A.global 
    A.liftIO $ mapM_ NS.disconnect left 
    A.liftIO $ mapM_ NS.disconnect right 
    A.liftIO $ mapM_ NS.disconnect pro

  NS.exit switch