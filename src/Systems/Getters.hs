{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}

module Systems.Getters (
  getAll,
  getEnemyPositions,
  getInnerWallPositions,
  getOuterWallPositions,
  getPlayerPosition
)
where 

import qualified Apecs      as A

import qualified Components as C


-- Copied from https://gitlab.com/dpwiz/spacemar/-/blob/master/src/Utils/Apecs.hs.
getAll
  :: forall a w m . (A.Get w m a, A.Members w m a)
  => A.SystemT w m [a]
getAll = reverse <$> A.cfold (flip (:)) []  -- TODO: Remove reverse?


getEnemyPositions :: C.System' [C.CPosition]
getEnemyPositions = do
  enemies :: [(C.CEnemy, C.CPosition)] <- getAll 
  return $ map snd enemies 


getInnerWallPositions :: C.System' [C.CPosition]
getInnerWallPositions = do 
  innerWalls :: [(C.CInnerWall, C.CPosition)] <- getAll 
  return $ map snd innerWalls 


getOuterWallPositions :: C.System' [C.CPosition] 
getOuterWallPositions = do 
  outerWalls :: [(C.COuterWall, C.CPosition)] <- getAll 
  return $ map snd outerWalls


getPlayerPosition :: C.System' C.CPosition
getPlayerPosition = do
  player :: [(C.CPlayer, C.CPosition)] <- getAll
  return $ (snd . head) player