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
eventHandler event =
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