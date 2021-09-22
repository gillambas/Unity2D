module Systems.Remove (
  removeBoard,
  removeDeadEnemies,
  removeEatenFood,
  removeDeadInnerWalls,
  removePointsChange
)
where 

import Control.Monad (when)

import qualified Apecs        as A 
import qualified Apecs.System as AS 

import qualified Components   as C 


removeBoard :: C.System' ()
removeBoard = do 
  removeEnemies
  removeFloor
  removeFood
  removeInnerWalls
  removeOuterWalls


removeEnemies :: C.System' ()
removeEnemies = A.cmap  $ \(C.CEnemy _) -> A.Not @C.EnemyComponents


removeFloor :: C.System' ()
removeFloor = A.cmap  $ \(C.CFloor _ ) -> A.Not @C.FloorComponents


removeFood :: C.System' ()
removeFood = A.cmap  $ \(C.CFood _ ) -> A.Not @C.FoodComponents


removeInnerWalls :: C.System' ()
removeInnerWalls = A.cmap  $ \(C.CInnerWall _) -> A.Not @C.InnerWallComponents


removeOuterWalls :: C.System' ()
removeOuterWalls = A.cmap  $ \(C.COuterWall _ ) -> A.Not @C.OuterWallComponents


-- | Remove enemies with low hp. 
removeDeadEnemies :: C.System' ()
removeDeadEnemies =
  AS.cmapIf (\(C.CHealth hp _ _) -> hp <= 0) (\(C.CEnemy _) -> A.Not @C.EnemyComponents)


-- | Remove inner walls with low hp.
removeDeadInnerWalls :: C.System' ()
removeDeadInnerWalls =
  AS.cmapIf (\(C.CHealth hp _ _) -> hp <= 0) (\(C.CInnerWall _) -> A.Not @C.InnerWallComponents)


-- | Remove food when the player consumes it.
removeEatenFood :: C.System' ()
removeEatenFood =
  A.cmapM_ $ \(C.CPlayer, posP :: C.CPosition) -> 
    A.cmapM_ $ \(C.CNutrition n, posF :: C.CPosition, etyF) -> 
      when (posP == posF) $ do 
        A.modify A.global (\(C.CFoodPoints fp) -> C.CFoodPoints (fp + n))
        A.set A.global $ C.CPointsChange (mconcat ["+", show n, "  "])
        A.destroy etyF (A.Proxy @C.FoodComponents)


-- | Clear the points change stream.
removePointsChange :: C.System' ()
removePointsChange = A.set A.global (mempty :: C.CPointsChange)