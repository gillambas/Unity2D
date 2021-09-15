module Systems.Initialise (
  initialise,
  boardSetup,
  setupScene
)
where

import Control.Monad    (replicateM)
import Data.Tuple.Extra ((&&&))
import Data.List        (zip4)

import qualified Apecs                as A
import qualified Data.Map             as Map
import qualified Linear               as L
import qualified System.Random        as R

import qualified Components           as C
import qualified Visualise.Animations as Anim


----------------------------------------------------------------------------------------------
-----------------------                   INITIALISE                   -----------------------
----------------------------------------------------------------------------------------------
-- | Initialise: Place the player, exit, outer walls, floor tiles, inner walls, enemies and food.
-- To be run at the beginning of each level.
initialise :: C.System' ()
initialise = do 
  config    <- A.get A.global 
  picBundle <- A.get A.global 

  A.set A.global (C.CFoodPoints (C.foodPoints config))
  A.set A.global (C.CLevel 10) -- TODO: Start from level 0

  A.newEntity_ (C.startPosition config, C.CPlayer, Anim.initPlayerIdleAnim picBundle)
  A.newEntity_ (C.exitPosition config, C.CExit) 

  boardSetup
  setupScene
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                   SETUP BOARD                  -----------------------
----------------------------------------------------------------------------------------------
-- | Create the outter walls and fill in the inner area with floors.
boardSetup :: C.System' ()
boardSetup = do
  outerBoarder <- createOuterBoarder
  innerArea    <- fillInnerArea

  mapM_ A.newEntity outerBoarder 
  mapM_ A.newEntity innerArea


createOuterBoarder :: C.System' [(C.CPosition, C.COuterWall)]
createOuterBoarder = do 
  config <- A.get A.global 

  let (xMin, yMin)  = C.bottomLeft config 
      (xMax, yMax)  = C.topRight config
      topBottom     = [ (x,y) | x <- [xMin .. xMax], y <- [yMin, yMax] ]  
      leftRight     = [ (x,y) | x <- [xMin, xMax], y <- [yMin+1 .. yMax-1] ]
      outerBoarder  = topBottom <> leftRight
      outerBoarder' = map (C.CPosition . uncurry L.V2) outerBoarder

  tiles <- A.liftIO $ replicateM (length outerBoarder) R.randomIO
  let tiles' = map C.COuterWall tiles

  return $ zip outerBoarder' tiles'


fillInnerArea :: C.System' [(C.CPosition, C.CFloor)]
fillInnerArea = do 
  config <- A.get A.global 

  let (xMin, yMin)  = C.bottomLeft config 
      (xMax, yMax)  = C.topRight config
      inner         = [ (x,y) | x <- [xMin+1 .. xMax-1], y <- [yMin+1 .. yMax-1] ]  
      inner'        = map (C.CPosition . uncurry L.V2) inner

  tiles <- A.liftIO $ replicateM (length inner) R.randomIO
  let tiles' = map C.CFloor tiles

  return $ zip inner' tiles'
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                   SETUP SCENE                  -----------------------
----------------------------------------------------------------------------------------------
-- | Add enemies, inner walls and food at random positions.
setupScene :: C.System' ()
setupScene = do 
  config     <- A.get A.global
  picBundle  <- A.get A.global
  C.CLevel l <- A.get A.global

  (innerWalls, foods, enemies) <- A.liftIO $ setupScene' config picBundle l

  mapM_ A.newEntity innerWalls
  mapM_ A.newEntity foods
  mapM_ A.newEntity enemies


setupScene' :: C.CConfig -> C.CPictureBundle -> Int -> IO ([C.InnerWallComponents], [C.FoodComponents], [C.EnemyComponents])
setupScene' 
  C.CConfig{C.bottomLeft, C.topRight, C.innerWallsRange, C.foodRange, C.innerWallHealth, C.nutrition} 
  picBundle@C.CPictureBundle{C.intactInnerWallPics, C.damagedInnerWallPics} 
  level = do

  -- Leave an outer layer empty so that the maze is solvable.
  let (xMin, yMin) = bottomLeft
      (xMax, yMax) = topRight
      minPosition  = L.V2 (xMin + 2) (yMin + 2)
      maxPosition  = L.V2 (xMax - 2) (yMax - 2)

  -- Place inner walls.
  nInnerWalls <- R.randomRIO innerWallsRange
  (innerWallPositions, occupiedPositions) <- randomUniques nInnerWalls minPosition maxPosition []
  innerWallTypes <- replicateM nInnerWalls R.randomIO
  let innerWallPositions' = map C.CPosition innerWallPositions
      innerWallTypes'     = map C.CInnerWall innerWallTypes
      innerWallPics       = map (uncurry C.CInnerWallPic . ((Map.!) intactInnerWallPics &&& (Map.!) damagedInnerWallPics)) innerWallTypes
      innerWallLives      = replicate nInnerWalls innerWallHealth
      innerWalls          = zip4 innerWallPositions' innerWallTypes' innerWallPics innerWallLives

  -- Place food.
  nFood <- R.randomRIO foodRange
  (foodPositions, occupiedPositions') <- randomUniques nFood minPosition maxPosition occupiedPositions
  foodTypes <- replicateM nFood R.randomIO
  let foodPositions' = map C.CPosition foodPositions
      foodTypes'     = map C.CFood foodTypes
      nutritions     = map (\f -> if f==C.Soda then C.soda nutrition else C.fruit nutrition) foodTypes
      nutritions'    = map C.CNutrition nutritions
      foods          = zip3 foodPositions' foodTypes' nutritions'

  -- Place enemies.
  let nEnemies = ceiling $ logBase 2.0 (fromIntegral level)
  (enemyPositions, _) <- randomUniques nEnemies minPosition maxPosition occupiedPositions'
  enemyTypes <- replicateM nEnemies R.randomIO
  let enemyPositions' = map C.CPosition enemyPositions
      enemyTypes'     = map C.CEnemy enemyTypes
      animations      = map (Anim.initEnemyIdleAnim picBundle) enemyTypes 
      enemyLives      = replicate nEnemies (C.CHealth 4 2) -- TODO: Read from config
      enemies         = zip4 enemyPositions' enemyTypes' animations enemyLives

  return (innerWalls, foods, enemies)
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------         GENERATE UNIQUE RANDOM ELEMENTS        -----------------------
----------------------------------------------------------------------------------------------
-- | Generate n random elements which are within the given range,
-- are different from the given list of elements and are different between them.
-- Returns the random elements generated and the list of elements updated to include the new elements.
randomUniques :: (Eq a, R.Random a) => Int -> a -> a -> [a] -> IO ([a], [a])
randomUniques n min max occupied = randomUniques' n min max ([], occupied)


randomUniques' :: (Eq a, R.Random a) => Int -> a -> a -> ([a], [a]) -> IO ([a], [a])
randomUniques' n min max rs@(news, occupied) = 
  if   n <= 0
  then return rs
  else do 
    (new, occupied') <- randomUnique min max occupied
    randomUniques' (n-1) min max (new : news, occupied')


-- | Generate a random element which is within the given range
-- and is different from the given list of elements.
-- Returns the random element generated and the list of element updated to include the new element.
randomUnique :: (Eq a, R.Random a) => a -> a -> [a] -> IO (a, [a])
randomUnique min max occupied = do
  r <- R.randomRIO (min, max)

  if   r `elem` occupied
  then randomUnique min max occupied
  else return (r, r:occupied)
----------------------------------------------------------------------------------------------