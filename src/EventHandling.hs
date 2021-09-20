module EventHandling (
  eventHandler
)
where

import System.Exit (exitSuccess)

import qualified Apecs                as A
import qualified Apecs.Gloss          as AG
import qualified Apecs.System         as AS
import qualified Linear               as L

import qualified Components           as C
import qualified Systems.Attack       as SAttack
import qualified Systems.Move         as SMove
import qualified Visualise.Animations as Anim


eventHandler :: AG.Event -> C.System' ()
eventHandler event = do
  C.CScreen screen <- A.get A.global

  case screen of 
    C.Game       -> handleGame event
    C.LevelIntro -> handleLevelIntro event
    C.GameOver   -> return ()


handleGame :: AG.Event -> C.System' ()
handleGame event = 
  case event of 
    -- Move player
    (AG.EventKey key@(AG.SpecialKey AG.KeyLeft ) AG.Down _ _) -> SMove.movePlayer key
    (AG.EventKey key@(AG.SpecialKey AG.KeyRight) AG.Down _ _) -> SMove.movePlayer key
    (AG.EventKey key@(AG.SpecialKey AG.KeyUp   ) AG.Down _ _) -> SMove.movePlayer key
    (AG.EventKey key@(AG.SpecialKey AG.KeyDown ) AG.Down _ _) -> SMove.movePlayer key

    -- Player attacks
    (AG.EventKey key@(AG.SpecialKey AG.KeySpace) AG.Down _ _) -> SAttack.playerAttack

    -- Terminate game
    (AG.EventKey (AG.SpecialKey AG.KeyEsc) AG.Down _ _) -> A.liftIO exitSuccess

    -- Unsupported key
    _ -> return ()


handleLevelIntro :: AG.Event -> C.System' ()
handleLevelIntro (AG.EventKey key@(AG.SpecialKey AG.KeyEnter) AG.Down _ _) = A.set A.global (C.CScreen C.Game)
handleLevelIntro _ = return () 
  
