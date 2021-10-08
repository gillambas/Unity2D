module HandleInput.Switch (
  connectSwitch,
  disconnectSwitch,
  handleSwitch
)
where 

import Control.Monad       ((>=>))
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

  C.CSwitchControllers leftContrs rightContrs <- A.get A.global 

  mapM_ (handleController millisecs) leftContrs
  mapM_ (handleController millisecs) rightContrs


handleController :: NS.HasInput t => Int -> NS.Controller t -> C.System' ()
handleController waitTime controller = do 
  input <- A.liftIO $ NS.getTimeoutInput waitTime controller
  whenJust input (interpretSwitchInput >=> handleSwitchInput)


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


connectSwitch :: NS.Console -> IO C.CSwitchControllers
connectSwitch console = do 
  leftCon  <- mapMM NS.connect (NS.getControllerInfos console)
  rightCon <- mapMM NS.connect (NS.getControllerInfos console)

  mapM_ (NS.setInputMode NS.Simple) leftCon
  mapM_ (NS.setInputMode NS.Simple) rightCon

  return C.CSwitchControllers
    { C.leftJoyCon    = leftCon
    , C.rightJoyCon   = rightCon }


disconnectSwitch :: C.System' ()
disconnectSwitch = do 
  C.CSwitchControllers left right <- A.get A.global 
  A.liftIO $ mapM_ NS.disconnect left 
  A.liftIO $ mapM_ NS.disconnect right 


-- Copied from Agda.Utils.Monad
-- (https://hackage.haskell.org/package/Agda-2.6.2/docs/Agda-Utils-Monad.html#v:mapMM)
mapMM :: (Traversable t, Monad m) => (a -> m b) -> m (t a) -> m (t b)
mapMM f mxs = mapM f =<< mxs