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
import qualified Systems.Movers       as SMove
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
    (AG.EventKey key@(AG.SpecialKey AG.KeySpace) AG.Down _ _) -> playerAttack

    -- Terminate game
    (AG.EventKey (AG.SpecialKey AG.KeyEsc) AG.Down _ _) -> A.liftIO exitSuccess

    -- Unsupported key
    _ -> return ()


playerAttack :: C.System' ()
playerAttack = do 
  A.cmapM $ \C.CPlayer -> Anim.initPlayerAttackAnim <$> A.get A.global
  inflictDamage


inflictDamage :: C.System' ()
inflictDamage =
  A.cmapM_ $ \(C.CPlayer, C.CPosition posP) ->
    AS.cmapIf
      (\(C.CPosition pos) -> posP + L.V2 1 0 == pos || posP + L.V2 0 1 == pos)
      (\(h@(C.CHealth hp damage), A.Not :: A.Not C.CPlayer) -> h {C.hp = hp - damage} )