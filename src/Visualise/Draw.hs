module Visualise.Draw ( 
  createBoardPicture,
  draw
)
where

import qualified Apecs           as A
import qualified Apecs.Gloss     as AG
import qualified Data.Map        as Map

import qualified Components      as C
import qualified Visualise.Tools as VTools


draw :: AG.Picture -> C.System' AG.Picture
draw boardPic = do 
  C.CScreen screen <- A.get A.global 

  case screen of 
    C.LevelIntro -> drawLevelIntro
    C.Game       -> drawGame boardPic
    C.GameOver   -> drawGameOver


drawLevelIntro :: C.System' AG.Picture
drawLevelIntro = do 
  level :: C.CLevel <- A.get A.global 
  return (AG.color AG.white . AG.scale 0.3 0.3 . AG.text . show $ level)


drawGameOver :: C.System' AG.Picture
drawGameOver = do 
  C.CLevel l <- A.get A.global
  let message = AG.color AG.white . AG.scale 0.3 0.3 . AG.text $ mconcat ["After ", show l, " days, you starved."]
  return message


drawGame :: AG.Picture -> C.System' AG.Picture
drawGame boardPic = do
  config    <- A.get A.global
  picBundle <- A.get A.global

  player     <- AG.foldDraw $ \(C.CPlayer, pos, C.CAnimation _ sprites index _) -> VTools.translate' (VTools.positionToCoords' pos) (sprites !! index)
  enemies    <- AG.foldDraw $ \(C.CEnemy _, pos, C.CAnimation _ sprites index _) -> VTools.translate' (VTools.positionToCoords' pos) (sprites !! index)
  food       <- AG.foldDraw $ \(C.CFood f, pos) -> VTools.translate' (VTools.positionToCoords' pos) (foodPic f picBundle)
  innerWalls <- AG.foldDraw $ \(C.CInnerWall _, pic, health, pos) -> VTools.translate' (VTools.positionToCoords' pos) (innerWallPic pic health)

  C.CFoodPoints   fp <- A.get A.global
  C.CPointsChange pc <- A.get A.global

  let foodPoints = AG.color AG.white . VTools.translate' (VTools.positionToCoords' (C.foodPointsPosition config)) . AG.scale 0.12 0.12 . AG.text $ mconcat [pc, "Food: ", show fp]
  
  return $ mconcat [boardPic, food, enemies, innerWalls, player, foodPoints]


innerWallPic :: C.CInnerWallPic -> C.CHealth -> AG.Picture
innerWallPic (C.CInnerWallPic intact damaged) (C.CHealth hp _ _) =
  if   hp <= 2 
  then damaged
  else intact


foodPic :: C.Food -> C.CPictureBundle -> AG.Picture
foodPic C.Fruit = C.fruitPic
foodPic C.Soda  = C.sodaPic


createBoardPicture :: C.System' AG.Picture
createBoardPicture = do
  picBundle <- A.get A.global

  outer <- AG.foldDraw $ \(C.COuterWall ow, pos) -> VTools.translate' (VTools.positionToCoords' pos) (C.outerWallPics picBundle Map.! ow)
  inner <- AG.foldDraw $ \(C.CFloor f, pos) -> VTools.translate' (VTools.positionToCoords' pos) (C.floorPics picBundle Map.! f)
  exit  <- AG.foldDraw $ \(C.CExit, pos) -> VTools.translate' (VTools.positionToCoords' pos) (C.exitPic picBundle)

  return $ mconcat [outer, inner, exit]