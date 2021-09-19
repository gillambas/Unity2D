module Systems.Remove (
  removeEnemies,
  removeFood,
  removeInnerWalls,
  removePointsChange
)
where 

import Control.Monad (when)

import qualified Apecs        as A 
import qualified Apecs.System as AS 

import qualified Components   as C 


-- | Remove enemies with low hp. 
removeEnemies :: C.System' ()
removeEnemies =
  AS.cmapIf (\(C.CHealth hp _ _) -> hp <= 0) (\(C.CEnemy _) -> A.Not @C.EnemyComponents)


-- | Remove food when the player consumes it.
removeFood :: C.System' ()
removeFood =
  A.cmapM_ $ \(C.CPlayer, posP :: C.CPosition) -> 
    A.cmapM_ $ \(C.CNutrition n, posF :: C.CPosition, etyF) -> 
      when (posP == posF) $ do 
        A.modify A.global (\(C.CFoodPoints fp) -> C.CFoodPoints (fp + n))
        A.set A.global $ C.CPointsChange (mconcat ["+", show n, "  "])
        A.destroy etyF (A.Proxy @C.FoodComponents)


-- | Remove inner walls with low hp.
removeInnerWalls :: C.System' ()
removeInnerWalls =
  AS.cmapIf (\(C.CHealth hp _ _) -> hp <= 0) (\(C.CInnerWall _) -> A.Not @C.InnerWallComponents)


-- | Doesn't remove, but rather clears the points change stream.
removePointsChange :: C.System' ()
removePointsChange = A.set A.global (mempty :: C.CPointsChange)