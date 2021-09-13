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


draw :: AG.Picture -> C.Config -> C.PictureBundle -> C.System' AG.Picture
draw boardPic C.Config{C.foodPointsPosition} picBundle = do
  player     <- AG.foldDraw $ \(C.CPlayer, pos, C.CAnimation _ sprites index _) -> VTools.translate' (VTools.positionToCoords' pos) (sprites !! index)
  enemies    <- AG.foldDraw $ \(C.CEnemy _, pos, C.CAnimation _ sprites index _) -> VTools.translate' (VTools.positionToCoords' pos) (sprites !! index)
  food       <- AG.foldDraw $ \(C.CFood f, pos) -> VTools.translate' (VTools.positionToCoords' pos) (foodPic f picBundle)
  innerWalls <- AG.foldDraw $ \(C.CInnerWall _, pic, life, pos) -> VTools.translate' (VTools.positionToCoords' pos) (innerWallPic pic life)

  C.CFoodPoints fp <- A.get A.global
  let foodPoints = AG.color AG.white . VTools.translate' (VTools.positionToCoords' foodPointsPosition) . AG.scale 0.12 0.12 . AG.text $ "Food " <> show fp
  
  return $ mconcat [boardPic, food, enemies, innerWalls, player, foodPoints]


innerWallPic :: C.CInnerWallPic -> C.CHealth -> AG.Picture
innerWallPic (C.CInnerWallPic intact damaged) (C.CHealth hp _) =
  if   hp <= 2 
  then damaged
  else intact


foodPic :: C.Food -> C.PictureBundle -> AG.Picture
foodPic C.Fruit = C.fruitPic
foodPic C.Soda  = C.sodaPic


createBoardPicture :: C.Config -> C.PictureBundle -> C.System' AG.Picture
createBoardPicture _ C.PictureBundle{C.exitPic, C.floorPics, C.outerWallPics} = do
  outer <- AG.foldDraw $ \(C.COuterWall ow, pos) -> VTools.translate' (VTools.positionToCoords' pos) (outerWallPics Map.! ow)
  inner <- AG.foldDraw $ \(C.CFloor f, pos) -> VTools.translate' (VTools.positionToCoords' pos) (floorPics Map.! f)
  exit  <- AG.foldDraw $ \(C.CExit, pos) -> VTools.translate' (VTools.positionToCoords' pos) exitPic

  return $ mconcat [outer, inner, exit]