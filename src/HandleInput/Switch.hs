-- | Functions needed for controlling the game with the Nintendo Switch controllers.

module HandleInput.Switch (
  -- * Connect
  connectSwitch,
  setSwitchComponent,
  -- * Read Switch input
  readSwitchInput,
  -- * Handle Switch input
  handleSwitchInput,
  -- * Disconnect
  --disconnectSwitch
)
where 

import Control.Monad       ((>=>))
import Control.Monad.Extra (whenJust)
import Data.Maybe          (catMaybes)
import System.Exit         (exitSuccess)
import System.IO.Unsafe    (unsafePerformIO)

import qualified Apecs                          as A
import qualified Apecs.Gloss                    as AG
import qualified Control.Concurrent.STM.TBQueue as TBQ
import qualified Control.Exception              as E
import qualified Control.Monad.STM              as STM
import qualified Device.Nintendo.Switch         as NS

import qualified Components                     as C
import qualified Systems.Attack                 as SAttack 
import qualified Systems.Initialise             as SInit
import qualified Systems.Move                   as SMove

{-
handleSwitch :: Float -> C.System' ()
handleSwitch dT = do 
  let millisecs = floor $ dT * 1000.0

  C.CSwitchControllers leftContrs rightContrs <- A.get A.global 

  mapM_ (handleController millisecs) leftContrs
  mapM_ (handleController millisecs) rightContrs


handleController :: NS.HasInput t => Int -> NS.Controller t -> C.System' ()
handleController waitTime controller = do 
  input <- A.liftIO $ NS.getTimeoutInput waitTime controller
  whenJust input (interpretSwitchInput >=> switchChangeWorld)
-}


----------------------------------------------------------------------------------------------
-----------------------                     CONNECT                    -----------------------
----------------------------------------------------------------------------------------------
-- | Set the global CSwitchInput component.
-- If controller connected create the TBQueue which will store the inputs.
-- If controller not connected set component to Nothing.
setSwitchComponent :: (Maybe (NS.Controller NS.LeftJoyCon), Maybe (NS.Controller NS.RightJoyCon)) -> C.System' ()
-- TODO: Handle all records correctly.
setSwitchComponent (leftCon, _) = do 
  let switchInput = case leftCon of 
                      Nothing -> C.CSwitchInput Nothing
                      _       -> C.CSwitchInput (Just (unsafePerformIO $ TBQ.newTBQueueIO 10))

  A.set A.global switchInput

  -- VERBOSE
  si :: C.CSwitchInput <- A.get A.global
  A.liftIO (print si)


-- | Connect at most one left and one right joy con.
connectSwitch :: NS.Console -> IO (Maybe (NS.Controller NS.LeftJoyCon), Maybe (NS.Controller NS.RightJoyCon))
connectSwitch console = do 
  leftCon  <- oneOrNone <$> mapMM safeConnect (NS.getControllerInfos @'NS.LeftJoyCon console)
  rightCon <- oneOrNone <$> mapMM safeConnect (NS.getControllerInfos @'NS.RightJoyCon console)

  -- VERBOSE
  whenJust leftCon  (\_ -> putStrLn "Left connected")
  whenJust rightCon (\_ -> putStrLn "Right connected")

  whenJust leftCon  (NS.setInputMode NS.Simple)
  whenJust rightCon (NS.setInputMode NS.Simple)

  return (leftCon, rightCon)


-- | Safe version of Device.Nintendo.Switch.connect.
-- Returns Nothing if no controller is detected.
safeConnect :: forall t. NS.HasCalibration t => NS.ControllerInfo t -> IO (Maybe (NS.Controller t))
safeConnect controllerInfo = do
  connection :: Either NS.ConnectionException (NS.Controller t) <- E.try $ NS.connect controllerInfo

  case connection of 
    Right c -> return (Just c)
    _       -> return Nothing


-- | If multiple controllers are connected keep only the first one (as this is a single-player game).
-- If no controllers are connected returns Nothing.
oneOrNone :: forall t. NS.HasCalibration t => [Maybe (NS.Controller t)] -> Maybe (NS.Controller t)
oneOrNone controllers = controller
  where 
    controllers' = catMaybes controllers
    controller   = case controllers' of 
      [] -> Nothing
      _  -> Just (head controllers')
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------               READ SWITCH INPUT                -----------------------
----------------------------------------------------------------------------------------------
-- | Read input sent from Switch controller and store in TBQueue
-- (if controller connected).
readSwitchInput :: NS.HasInput t => Maybe (NS.Controller t) -> C.System' ()
readSwitchInput Nothing = return ()
readSwitchInput (Just controller) = do 
  C.CSwitchInput leftQ rightQ proQ <- A.get A.global
  -- TODO: Handle other queues.
  whenJust leftQ (\q -> do
    input <- A.liftIO $ NS.getInput controller 
    liftAtomically (TBQ.writeTBQueue q input) )
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------              HANDLE SWITCH INPUT               -----------------------
----------------------------------------------------------------------------------------------
-- | Read topmost input stored in TBQueue, interpret it according to game rules
-- and change the game world accordingly.
handleSwitchInput :: TBQ.TBQueue NS.Input -> C.System' ()
handleSwitchInput inputQueue = do 
  input <- liftAtomically (TBQ.readTBQueue inputQueue)
  interpretedInput <- interpretSwitchInput input
  changeWorld interpretedInput


-- | Interpret input from Switch controller according to game rules.
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


-- | Auxiliary to interpretSwitchInput.
-- Only discrete stick directions are recognised.
interpretStickDirection :: NS.StickDirection a -> C.SwitchInput
interpretStickDirection (NS.Analog _ _) = C.None
interpretStickDirection (NS.Discrete d) = case d of
  NS.None  -> C.None
  NS.Left  -> C.Left
  NS.Up    -> C.Up
  NS.Right -> C.Right
  NS.Down  -> C.Down
  _        -> C.None


-- | Change the game world based on the input of the Switch controller.
changeWorld :: C.SwitchInput -> C.System' ()
changeWorld = \case 
  C.Up      -> SMove.movePlayer AG.KeyUp
  C.Down    -> SMove.movePlayer AG.KeyDown
  C.Left    -> SMove.movePlayer AG.KeyLeft
  C.Right   -> SMove.movePlayer AG.KeyRight
  C.Attack  -> SAttack.playerAttack
  C.Restart -> SInit.startNewGame
  C.Exit    -> A.liftIO exitSuccess
  C.None    -> return ()
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                   DISCONNECT                   -----------------------
----------------------------------------------------------------------------------------------
{-
disconnectSwitch :: C.System' ()
disconnectSwitch = do 
  C.CSwitchControllers left right <- A.get A.global 
  A.liftIO $ mapM_ NS.disconnect left 
  A.liftIO $ mapM_ NS.disconnect right 
-}
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------              AUXILIARY FUNCTIONS               -----------------------
----------------------------------------------------------------------------------------------
-- Copied from Agda.Utils.Monad
-- (https://hackage.haskell.org/package/Agda-2.6.2/docs/Agda-Utils-Monad.html#v:mapMM)
mapMM :: (Traversable t, Monad m) => (a -> m b) -> m (t a) -> m (t b)
mapMM f mxs = mapM f =<< mxs


-- | Convenience function.
-- Execute an STM transaction atomically and lift the results in the System' monad.
liftAtomically :: STM.STM a -> C.System' a 
liftAtomically = A.liftIO . STM.atomically
----------------------------------------------------------------------------------------------