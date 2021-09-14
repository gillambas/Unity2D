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
import qualified Systems.Getters      as SGet
import qualified Visualise.Animations as Anim


eventHandler :: AG.Event -> C.System' ()
eventHandler event =
  case event of 
    -- Move player
    (AG.EventKey key@(AG.SpecialKey AG.KeyLeft ) AG.Down _ _) -> movePlayer key
    (AG.EventKey key@(AG.SpecialKey AG.KeyRight) AG.Down _ _) -> movePlayer key
    (AG.EventKey key@(AG.SpecialKey AG.KeyUp   ) AG.Down _ _) -> movePlayer key
    (AG.EventKey key@(AG.SpecialKey AG.KeyDown ) AG.Down _ _) -> movePlayer key

    -- Player attacks
    (AG.EventKey key@(AG.SpecialKey AG.KeySpace) AG.Down _ _) -> A.cmapM $ \C.CPlayer -> Anim.initPlayerAttackAnim <$> A.get A.global

    -- Terminate game
    (AG.EventKey (AG.SpecialKey AG.KeyEsc) AG.Down _ _) -> A.liftIO exitSuccess

    -- Unsupported key
    _ -> return ()


movePlayer :: AG.Key -> C.System' ()
movePlayer key = do
  enemies    <- SGet.getEnemyPositions
  innerWalls <- SGet.getInnerWallPositions
  outerWalls <- SGet.getOuterWallPositions 

  let occupiedPositions = enemies <> innerWalls <> outerWalls
      displacement = case key of 
        AG.SpecialKey AG.KeyLeft  -> C.CPosition $ L.V2 (-1) 0
        AG.SpecialKey AG.KeyRight -> C.CPosition $ L.V2 1 0 
        AG.SpecialKey AG.KeyUp    -> C.CPosition $ L.V2 0 1 
        AG.SpecialKey AG.KeyDown  -> C.CPosition $ L.V2 0 (-1)
        _                         -> C.CPosition $ L.V2 0 0

  AS.cmapIf
    (\(pos, C.CFoodPoints fp) -> (pos + displacement) `notElem` occupiedPositions && fp > 0)
    (\(C.CPlayer, pos, C.CFoodPoints fp) -> (pos + displacement, C.CFoodPoints (fp-1)))