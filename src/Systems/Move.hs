module Systems.Move (
  moveEnemy,
  movePlayer
)
where 

import Control.Monad (unless, when)

import qualified Apecs        as A
import qualified Apecs.Gloss  as AG
import qualified Apecs.System as AS
import qualified Linear       as L

import qualified Components   as C
import qualified Systems.Get  as SGet


moveEnemy :: C.System' ()
moveEnemy = do 
  C.CSkipMove sm <- A.get A.global

  unless sm $ do 
    outerWallsPos <- SGet.getOuterWallPositions

    A.cmapM_ $ \(C.CEnemy _, enemyPos@(C.CPosition enemyPos'@(L.V2 xE yE)), ety) -> do
      -- Get these positions at each iteration as they may change at run-time.
      innerWallsPos                        <- SGet.getInnerWallPositions
      enemiesPos                           <- SGet.getEnemyPositions
      playerPos@(C.CPosition (L.V2 xP yP)) <- SGet.getPlayerPosition

      let prohibited = mconcat [outerWallsPos, innerWallsPos, enemiesPos, [playerPos]]

      let displacement 
            | xP == xE  = if yP > yE then L.V2 0 1 else L.V2 0 (-1)
            | xP > xE   = L.V2 1 0
            | otherwise = L.V2 (-1) 0

          potentialPos = C.CPosition (enemyPos' + displacement)

      when
        (potentialPos `notElem` prohibited)
        (A.set ety potentialPos)

  A.set A.global (C.CSkipMove (not sm))


movePlayer :: AG.SpecialKey -> C.System' ()
movePlayer key = do
  enemies    <- SGet.getEnemyPositions
  innerWalls <- SGet.getInnerWallPositions
  outerWalls <- SGet.getOuterWallPositions 

  let occupiedPositions = enemies <> innerWalls <> outerWalls
      displacement = case key of 
        AG.KeyLeft  -> C.CPosition $ L.V2 (-1) 0
        AG.KeyRight -> C.CPosition $ L.V2 1 0 
        AG.KeyUp    -> C.CPosition $ L.V2 0 1 
        AG.KeyDown  -> C.CPosition $ L.V2 0 (-1)
        _           -> C.CPosition $ L.V2 0 0

  AS.cmapIf
    (\(pos, C.CFoodPoints fp) -> (pos + displacement) `notElem` occupiedPositions && fp > 0)
    (\(C.CPlayer, pos, C.CFoodPoints fp) -> (pos + displacement, C.CFoodPoints (fp-1)))