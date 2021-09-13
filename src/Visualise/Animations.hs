module Visualise.Animations (
  initEnemyIdleAnim,
  initPlayerAttackAnim,
  initPlayerIdleAnim,
  overrideFiniteAnimations,
  stepAnimation
)
where 

import qualified Apecs.System as AS

import qualified Components   as C


-- | Step an animation's index to point to the next sprite.
stepAnimation :: C.CAnimation -> C.CAnimation
stepAnimation a@(C.CAnimation _ sprites index _) = 
  let index' = (index + 1) `mod` length sprites
  in  a {C.index = index'} 


initPlayerIdleAnim :: C.PictureBundle -> C.CAnimation
initPlayerIdleAnim picBundle = C.CAnimation 0.25 (C.playerIdlePics picBundle) 0 False


initEnemyIdleAnim :: C.PictureBundle -> C.Enemy -> C.CAnimation
initEnemyIdleAnim picBundle C.Vampire = C.CAnimation 0.25 (C.vampireIdlePics picBundle) 0 False
initEnemyIdleAnim picBundle C.Zombie  = C.CAnimation 0.25 (C.zombieIdlePics picBundle) 0 False


initPlayerAttackAnim :: C.PictureBundle -> C.CAnimation
initPlayerAttackAnim picBundle = C.CAnimation 0.75 (C.playerAttackPics picBundle) 0 True


isAnimOver :: C.CAnimation -> Bool 
isAnimOver (C.CAnimation _ sprites index isFinite) = 
  isFinite && index == length sprites - 1


overrideFiniteAnimations :: C.PictureBundle -> C.System' ()
overrideFiniteAnimations picBundle = do 
  AS.cmapIf isAnimOver (\(C.CEnemy e) -> initEnemyIdleAnim picBundle e)
  AS.cmapIf isAnimOver (\C.CPlayer    -> initPlayerIdleAnim picBundle)