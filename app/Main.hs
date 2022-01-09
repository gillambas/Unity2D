module Main where

import qualified Apecs                  as A
import qualified Apecs.Gloss            as AG
import qualified Apecs.STM              as ASTM
import qualified Device.Nintendo.Switch as NS

import qualified Components             as C
import qualified HandleInput.Keyboard   as Keyboard
import qualified HandleInput.Switch     as Switch
import qualified Step                   as Step
import qualified Systems.Initialise     as SInit
import qualified Visualise.Draw         as Draw
import qualified Visualise.Load         as Load


main :: IO ()
main =
  NS.withConsole $ \switch -> do 
    graphics <- Load.loadGraphics
    controllers@(leftCon, rightCon, proCon) <- Switch.connectSwitch switch 
    
    w <- C.initWorld

    A.runWith w $ do
      A.set A.global graphics

      Switch.setSwitchComponent controllers

      -- TODO: New threads for other records of component?
      ASTM.forkSys $ Switch.readSwitchInput rightCon
      
      SInit.startNewGame

      AG.play
        AG.FullScreen
        AG.black
        60
        Draw.draw
        Keyboard.eventHandler
        Step.stepper

      --Switch.disconnectSwitch