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
initialise :: C.Config -> C.PictureBundle -> C.System' ()
initialise 
  cfg@C.Config{C.bottomLeft, C.topRight, C.exitPosition, C.startPosition, C.foodPoints} 
  picBundle = do 

  A.set A.global (C.CFoodPoints foodPoints)
  A.set A.global (C.CLevel 10) -- TODO: Start from level 0

  A.newEntity_ (startPosition, C.CPlayer, Anim.initPlayerIdleAnim picBundle)
  A.newEntity_ (exitPosition, C.CExit) 

  boardSetup bottomLeft topRight
  setupScene cfg picBundle
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                   SETUP BOARD                  -----------------------
----------------------------------------------------------------------------------------------
-- | Create the outter walls and fill in the inner area with floors.
boardSetup :: (Int, Int) -> (Int, Int) -> C.System' ()
boardSetup min max = do
  outerBoarder <- A.liftIO $ createOuterBoarder min max
  innerArea    <- A.liftIO $ fillInnerArea min max

  mapM_ A.newEntity outerBoarder 
  mapM_ A.newEntity innerArea


createOuterBoarder :: (Int, Int) -> (Int, Int) -> IO [(C.CPosition, C.COuterWall)]
createOuterBoarder (xMin, yMin) (xMax, yMax) = do 
  let topBottom     = [ (x,y) | x <- [xMin .. xMax], y <- [yMin, yMax] ]  
      leftRight     = [ (x,y) | x <- [xMin, xMax], y <- [yMin+1 .. yMax-1] ]
      outerBoarder  = topBottom <> leftRight
      outerBoarder' = map (C.CPosition . uncurry L.V2) outerBoarder

  tiles <- replicateM (length outerBoarder) R.randomIO
  let tiles' = map C.COuterWall tiles

  return $ zip outerBoarder' tiles'


fillInnerArea :: (Int, Int) -> (Int, Int) -> IO [(C.CPosition, C.CFloor)]
fillInnerArea (xMin, yMin) (xMax, yMax) = do 
  let inner  = [ (x,y) | x <- [xMin + 1 .. xMax - 1], y <- [yMin + 1 .. yMax - 1] ]  
      inner' = map (C.CPosition . uncurry L.V2) inner

  tiles <- replicateM (length inner) R.randomIO
  let tiles' = map C.CFloor tiles

  return $ zip inner' tiles'
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                   SETUP SCENE                  -----------------------
----------------------------------------------------------------------------------------------
-- | Add enemies, inner walls and food at random positions.
setupScene :: C.Config -> C.PictureBundle -> C.System' ()
setupScene cfg picBundle = do 
  C.CLevel l <- A.get A.global

  (innerWalls, foods, enemies) <- A.liftIO $ setupScene' cfg picBundle l

  mapM_ A.newEntity innerWalls
  mapM_ A.newEntity foods
  mapM_ A.newEntity enemies


setupScene' :: C.Config -> C.PictureBundle -> Int -> IO ([C.InnerWallComponents], [C.FoodComponents], [C.EnemyComponents])
setupScene' 
  C.Config{C.bottomLeft, C.topRight, C.innerWallsRange, C.foodRange, C.innerWallHealth, C.nutrition} 
  picBundle@C.PictureBundle{C.intactInnerWallPics, C.damagedInnerWallPics} 
  level = do

  -- Leave an outer layer empty so that the maze is solvable.
  let (xMin, yMin) = bottomLeft
      (xMax, yMax) = topRight
      minPosition  = L.V2 (xMin + 2) (yMin + 2)
      maxPosition  = L.V2 (xMax - 2) (yMax - 2)

  -- Place inner walls
  nInnerWalls <- R.randomRIO innerWallsRange
  (innerWallPositions, occupiedPositions) <- randomPositions nInnerWalls minPosition maxPosition []
  innerWallTypes <- replicateM nInnerWalls R.randomIO
  let innerWallPositions' = map C.CPosition innerWallPositions
      innerWallTypes'     = map C.CInnerWall innerWallTypes
      innerWallPics       = map (uncurry C.CInnerWallPic . ((Map.!) intactInnerWallPics &&& (Map.!) damagedInnerWallPics)) innerWallTypes
      innerWallLives      = replicate nInnerWalls innerWallHealth
      innerWalls          = zip4 innerWallPositions' innerWallTypes' innerWallPics innerWallLives

  -- Place food 
  nFood <- R.randomRIO foodRange
  (foodPositions, occupiedPositions') <- randomPositions nFood minPosition maxPosition occupiedPositions
  foodTypes <- replicateM nFood R.randomIO
  let foodPositions' = map C.CPosition foodPositions
      foodTypes'     = map C.CFood foodTypes
      nutritions     = map (\f -> if f==C.Soda then C.soda nutrition else C.fruit nutrition) foodTypes
      nutritions'    = map C.CNutrition nutritions
      foods          = zip3 foodPositions' foodTypes' nutritions'

  -- Place enemies
  let nEnemies = ceiling $ logBase 2.0 (fromIntegral level)
  (enemyPositions, _) <- randomPositions nEnemies minPosition maxPosition occupiedPositions'
  enemyTypes <- replicateM nEnemies R.randomIO
  let enemyPositions' = map C.CPosition enemyPositions
      enemyTypes'     = map C.CEnemy enemyTypes
      animations      = map (Anim.initEnemyIdleAnim picBundle) enemyTypes 
      enemyLives      = replicate nEnemies (C.CHealth 4 2) -- TODO: Read from config
      enemies         = zip4 enemyPositions' enemyTypes' animations enemyLives

  return (innerWalls, foods, enemies)
----------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------
-----------------------                RANDOM POSITIONS                -----------------------
----------------------------------------------------------------------------------------------
-- | Generate n random positions which are within the rectangle formed by the given positions,
-- are different from the given occupied positions and are different between them.
-- Returns the random positions generated and the occupied positions updated to include the new positions.
randomPositions :: Int -> C.Position -> C.Position -> [C.Position] -> IO ([C.Position], [C.Position])
randomPositions n min max occupiedPositions = randomPositions' n min max ([], occupiedPositions)


randomPositions' :: Int -> C.Position -> C.Position -> ([C.Position], [C.Position]) -> IO ([C.Position], [C.Position])
randomPositions' 0 _ _ ls = return ls
randomPositions' n min max (newPositions, occupiedPositions) = do
  (newPosition, occupiedPositions') <- randomPosition min max occupiedPositions
  randomPositions' (n-1) min max (newPosition : newPositions, occupiedPositions')


-- | Generate a random position which is within the rectangle formed by the given positions
-- and is different from the given occupied positions.
-- Returns the random position generated and the occupied positions updated to include the new position.
randomPosition :: C.Position -> C.Position -> [C.Position] -> IO (C.Position, [C.Position])
randomPosition min@(L.V2 xMin yMin) max@(L.V2 xMax yMax) occupiedPositions = do
  x <- R.randomRIO (xMin, xMax)
  y <- R.randomRIO (yMin, yMax)
  let l = L.V2 x y

  if   l `elem` occupiedPositions
  then randomPosition min max occupiedPositions
  else return (l, l:occupiedPositions)
----------------------------------------------------------------------------------------------