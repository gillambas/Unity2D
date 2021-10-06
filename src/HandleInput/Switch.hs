{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module HandleInput.Switch (
  handleSwitch
)
where 

import Control.Monad       (forM_, (>=>))
import Control.Monad.Extra (whenJust)
import System.Exit         (exitSuccess)

import qualified Apecs                  as A
import qualified Apecs.Gloss            as AG
import qualified Device.Nintendo.Switch as NS

import qualified Components             as C
import qualified Systems.Attack         as SAttack 
import qualified Systems.Initialise     as SInit
import qualified Systems.Move           as SMove


handleSwitch :: Float -> C.System' ()
handleSwitch dT = do 
  let millisecs = floor $ dT * 1000.0

  switch <- A.liftIO NS.init 

  infosL <- A.liftIO $ NS.getControllerInfos @'NS.LeftJoyCon switch

  forM_ infosL $ \info -> do 
    controller <- A.liftIO $ NS.connect info

    input <- A.liftIO $ NS.getTimeoutInput millisecs controller

    whenJust input (interpretSwitchInput >=> handleSwitchInput)

    A.liftIO $ NS.disconnect controller

  A.liftIO $ NS.exit switch 


handleSwitchInput :: C.SwitchInput -> C.System' ()
handleSwitchInput = \case 
  C.Up      -> SMove.movePlayer AG.KeyUp
  C.Down    -> SMove.movePlayer AG.KeyDown
  C.Left    -> SMove.movePlayer AG.KeyLeft
  C.Right   -> SMove.movePlayer AG.KeyRight
  C.Attack  -> SAttack.playerAttack
  C.Restart -> SInit.startNewGame
  C.Exit    -> A.liftIO exitSuccess
  C.None    -> return ()


interpretSwitchInput :: NS.Input -> C.System' C.SwitchInput 
interpretSwitchInput input = do 
  C.CScreen screen <- A.get A.global

  let switchInput 
        | NS.btnUp      input = if screen == C.Game     then C.Up      else C.None
        | NS.btnDown    input = if screen == C.Game     then C.Down    else C.None
        | NS.btnLeft    input = if screen == C.Game     then C.Left    else C.None
        | NS.btnRight   input = if screen == C.Game     then C.Right   else C.None
        | NS.btnA       input = if screen == C.Game     then C.Attack  else C.None
        | NS.btnMinus   input = if screen == C.GameOver then C.Restart else C.None
        | NS.btnHome    input = C.Exit
        | NS.stickLeft  input /= NS.Discrete NS.None = if screen == C.Game then interpretStickDirection (NS.stickLeft  input) else C.None 
        | NS.stickRight input /= NS.Discrete NS.None = if screen == C.Game then interpretStickDirection (NS.stickRight input) else C.None
        | otherwise = C.None

  return switchInput


interpretStickDirection :: NS.StickDirection a -> C.SwitchInput
interpretStickDirection (NS.Analog _ _) = C.None
interpretStickDirection (NS.Discrete d) = case d of
  NS.None  -> C.None
  NS.Left  -> C.Left
  NS.Up    -> C.Up
  NS.Right -> C.Right
  NS.Down  -> C.Down
  _        -> C.None