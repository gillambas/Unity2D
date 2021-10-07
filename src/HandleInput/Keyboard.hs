module HandleInput.Keyboard (
  eventHandler
)
where

import System.Exit (exitSuccess)

import qualified Apecs                as A
import qualified Apecs.Gloss          as AG

import qualified Components           as C
import qualified Systems.Attack       as SAttack
import qualified Systems.Initialise   as SInit
import qualified Systems.Move         as SMove


eventHandler :: AG.Event -> C.System' ()
eventHandler event = do
  C.CScreen screen <- A.get A.global

  case screen of 
    C.Game       -> handleGame event
    C.LevelIntro -> handleLevelIntro event
    C.GameOver   -> handleGameOver event


handleGame :: AG.Event -> C.System' ()
handleGame = \case
  -- Move player
  (AG.EventKey (AG.SpecialKey AG.KeyLeft ) AG.Down _ _) -> SMove.movePlayer AG.KeyLeft
  (AG.EventKey (AG.SpecialKey AG.KeyRight) AG.Down _ _) -> SMove.movePlayer AG.KeyRight
  (AG.EventKey (AG.SpecialKey AG.KeyUp   ) AG.Down _ _) -> SMove.movePlayer AG.KeyUp
  (AG.EventKey (AG.SpecialKey AG.KeyDown ) AG.Down _ _) -> SMove.movePlayer AG.KeyDown

  -- Player attacks
  (AG.EventKey (AG.SpecialKey AG.KeySpace) AG.Down _ _) -> SAttack.playerAttack

  -- Terminate game
  (AG.EventKey (AG.SpecialKey AG.KeyEsc  ) AG.Down _ _) -> A.liftIO exitSuccess

  -- Unsupported key
  _                                                     -> return ()


handleLevelIntro :: AG.Event -> C.System' ()
handleLevelIntro = \case
  (AG.EventKey (AG.SpecialKey AG.KeyEnter) AG.Down _ _) -> A.set A.global (C.CScreen C.Game)
  (AG.EventKey (AG.SpecialKey AG.KeyEsc  ) AG.Down _ _) -> A.liftIO exitSuccess
  _                                                     -> return () 
  

handleGameOver :: AG.Event -> C.System' ()
handleGameOver = \case
  (AG.EventKey (AG.SpecialKey AG.KeyEnter) AG.Down _ _) -> SInit.startNewGame
  (AG.EventKey (AG.SpecialKey AG.KeyEsc  ) AG.Down _ _) -> A.liftIO exitSuccess
  _                                                     -> return ()