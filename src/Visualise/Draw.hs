module Visualise.Draw ( 
  createBoardPicture,
  draw
)
where

import qualified Apecs               as A
import qualified Apecs.Gloss         as AG
import qualified Data.Map            as Map
import qualified Graphics.Rasterific as Rast
import qualified Linear              as L

import qualified Components          as C
import qualified Visualise.Tools     as VTools


draw :: C.System' AG.Picture
draw = do 
  C.CScreen screen <- A.get A.global 

  case screen of 
    C.LevelIntro -> drawLevelIntro
    C.Game       -> drawGame
    C.GameOver   -> drawGameOver


drawLevelIntro :: C.System' AG.Picture
drawLevelIntro = do 
  level :: C.CLevel <- A.get A.global 
  VTools.drawText (200, 45) (Rast.V2 10 35) (Rast.PointSize 16) (show level)


drawGameOver :: C.System' AG.Picture
drawGameOver = do 
  C.CLevel l <- A.get A.global

  let message = if   l == 1
                then mconcat ["After ", show l, " day, you starved."]
                else mconcat ["After ", show l, " days, you starved."]

  VTools.drawText (650, 45) (Rast.V2 10 35) (Rast.PointSize 16) message


drawGame :: C.System' AG.Picture
drawGame = do
  graphics                 <- A.get A.global
  C.CBoardPicture boardPic <- A.get A.global

  player     <- AG.foldDraw $ \(C.CPlayer, pos, C.CAnimation _ sprites index _) -> VTools.translate' (VTools.positionToCoords' pos) (sprites !! index)
  enemies    <- AG.foldDraw $ \(C.CEnemy _, pos, C.CAnimation _ sprites index _) -> VTools.translate' (VTools.positionToCoords' pos) (sprites !! index)
  food       <- AG.foldDraw $ \(C.CFood f, pos) -> VTools.translate' (VTools.positionToCoords' pos) (foodPic f graphics)
  innerWalls <- AG.foldDraw $ \(C.CInnerWall _, pic, health, pos) -> VTools.translate' (VTools.positionToCoords' pos) (innerWallPic pic health)

  C.CFoodPoints   fp <- A.get A.global
  C.CPointsChange pc <- A.get A.global

  let foodPointsPosition = if   pc == mempty
                           then C.CPosition $ L.V2 6 (-1)
                           else C.CPosition $ L.V2 5 (-1)

  foodPoints <- VTools.translate' (VTools.positionToCoords' foodPointsPosition) 
                <$> VTools.drawText (325, 45) (Rast.V2 10 35) (Rast.PointSize 12) (mconcat [pc, "Food: ", show fp])
  
  return $ mconcat [boardPic, food, enemies, innerWalls, player, foodPoints]


innerWallPic :: C.CInnerWallPic -> C.CHealth -> AG.Picture
innerWallPic (C.CInnerWallPic intact damaged) (C.CHealth hp _ _) =
  if   hp <= 2 
  then damaged
  else intact


foodPic :: C.Food -> C.CGraphics -> AG.Picture
foodPic C.Fruit = C.fruitPic
foodPic C.Soda  = C.sodaPic


createBoardPicture :: C.System' ()
createBoardPicture = do
  graphics <- A.get A.global

  outer <- AG.foldDraw $ \(C.COuterWall ow, pos) -> VTools.translate' (VTools.positionToCoords' pos) (C.outerWallPics graphics Map.! ow)
  inner <- AG.foldDraw $ \(C.CFloor f, pos) -> VTools.translate' (VTools.positionToCoords' pos) (C.floorPics graphics Map.! f)
  exit  <- AG.foldDraw $ \(C.CExit, pos) -> VTools.translate' (VTools.positionToCoords' pos) (C.exitPic graphics)

  let boardPic = mconcat [outer, inner, exit]

  A.set A.global (C.CBoardPicture boardPic)