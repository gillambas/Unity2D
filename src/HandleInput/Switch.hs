-- | Functions needed for controlling the game with the Nintendo Switch controllers.

{-# LANGUAGE KindSignatures #-}

module HandleInput.Switch (
  -- * Connect
  connectControllers,
  -- * Set component
  setCSwitchInput,
  -- * Read Switch input
  readLeftInput,
  readProInput,
  readRightInput,
  -- * Handle Switch input
  handleSwitchInput,
  -- * Disconnect
  disconnectSwitch
)
where 

import Control.Monad       (forever)
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


import qualified Apecs.STM as ASTM

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
-- | Connect at most one of each: left joy, right joy con and pro controller.
connectControllers 
  :: NS.Console 
  -> IO ( Maybe (NS.Controller NS.LeftJoyCon)
        , Maybe (NS.Controller NS.RightJoyCon)
        , Maybe (NS.Controller NS.ProController) )
connectControllers console = do 
  leftCon  <- connectController console 
  rightCon <- connectController console
  proCon   <- connectController console

  whenJust leftCon  (NS.setInputMode NS.Simple)
  whenJust rightCon (NS.setInputMode NS.Simple)

  -- VERBOSE
  maybe (putStrLn "Left joy con not connected")   (\_ -> putStrLn "Left joy con connected")   leftCon
  maybe (putStrLn "Right joy con not connected")  (\_ -> putStrLn "Right joy con connected")  rightCon
  maybe (putStrLn "Pro controller not connected") (\_ -> putStrLn "Pro controller connected") rightCon

  return (leftCon, rightCon, proCon)


-- | Connect one controller (left/right/pro).
-- Auxiliary to connectControllers.
connectController :: forall t. (NS.HasCalibration t, NS.IsController t) => NS.Console -> IO (Maybe (NS.Controller t))
connectController console = oneOrNone <$> mapMM safeConnect (NS.getControllerInfos console)


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
-----------------------                  SET COMPONENT                 -----------------------
----------------------------------------------------------------------------------------------
-- | Set the global CSwitchInput component.
-- If controller(s) connected create the TBQueue(s) which will store the inputs.
-- If controller(s) not connected set component field(s) to Nothing.
setCSwitchInput 
  :: ( Maybe (NS.Controller NS.LeftJoyCon)
     , Maybe (NS.Controller NS.RightJoyCon)
     , Maybe (NS.Controller NS.ProController) )
  -> C.System' ()
setCSwitchInput (leftCon, rightCon, proCon) = do 
  let leftComp  = setCSwitchInput' leftCon
      rightComp = setCSwitchInput' rightCon
      proComp   = setCSwitchInput' proCon
      component = C.CSwitchInput leftComp rightComp proComp
 
  A.set A.global component

  -- VERBOSE
  si :: C.CSwitchInput <- A.get A.global
  A.liftIO (print si)


-- | Set field (left/right/pro) of global CSwitchInput component.
-- Auxiliary to setCSwitchInput.
setCSwitchInput' :: Maybe (NS.Controller t) -> Maybe (TBQ.TBQueue NS.Input)
setCSwitchInput' = maybe Nothing (const (Just (unsafePerformIO $ TBQ.newTBQueueIO 10)))
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------               READ SWITCH INPUT                -----------------------
----------------------------------------------------------------------------------------------
-- | Read input sent from Left joy con and store in respective TBQueue (if controller connected).
readLeftInput :: Maybe (NS.Controller NS.LeftJoyCon) -> C.System' ()
readLeftInput Nothing = return ()
readLeftInput (Just controller) = do 
  C.CSwitchInput queue _ _ <- A.get A.global
  readControllerInput controller queue


-- | Read input sent from Right joy con and store in respective TBQueue (if controller connected).
readRightInput :: Maybe (NS.Controller NS.RightJoyCon) -> C.System' ()
readRightInput Nothing = return ()
readRightInput (Just controller) = do 
  C.CSwitchInput _ queue _ <- A.get A.global
  readControllerInput controller queue


-- | Read input sent from Pro controller and store in respective TBQueue (if controller connected).
readProInput :: Maybe (NS.Controller NS.ProController) -> C.System' ()
readProInput Nothing = return ()
readProInput (Just controller) = do 
  C.CSwitchInput _ _ queue <- A.get A.global
  readControllerInput controller queue


-- | Read input and write TBQueue for arbitrary controller.
-- Auxiliary to readLeftInput, readRightInput & readProInput.
readControllerInput :: NS.HasInput t => NS.Controller t -> Maybe (TBQ.TBQueue NS.Input) -> C.System' ()
readControllerInput _ Nothing = return ()
readControllerInput controller (Just queue) = forever $ do 
  ASTM.threadDelay 1000000
  input <- A.liftIO $ NS.getInput controller 
  A.liftIO $ print input -- VERBOSE
  liftAtomically (TBQ.writeTBQueue queue input)
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

  -- VERBOSE
  A.liftIO $ print interpretedInput

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
disconnectSwitch
  :: ( Maybe (NS.Controller NS.LeftJoyCon)
     , Maybe (NS.Controller NS.RightJoyCon)
     , Maybe (NS.Controller NS.ProController) )
  -> IO ()
disconnectSwitch (leftCon, rightCon, proCon) = do 
  whenJust leftCon  NS.disconnect
  whenJust rightCon NS.disconnect
  whenJust proCon   NS.disconnect
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