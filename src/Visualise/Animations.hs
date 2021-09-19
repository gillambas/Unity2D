module Visualise.Animations (
  initEnemyAttackAnim,
  initEnemyIdleAnim,
  initPlayerAttackAnim,
  initPlayerIdleAnim,
  overrideFiniteAnimations,
  stepAnimation
)
where 

import qualified Apecs        as A
import qualified Apecs.System as AS

import qualified Components   as C


initEnemyAttackAnim :: C.CPictureBundle -> C.Enemy -> C.CAnimation
initEnemyAttackAnim picBundle C.Vampire = C.CAnimation 0.5 (C.vampireAttackPics picBundle) 0 True
initEnemyAttackAnim picBundle C.Zombie  = C.CAnimation 0.5 (C.zombieAttackPics picBundle) 0 True


initEnemyIdleAnim :: C.CPictureBundle -> C.Enemy -> C.CAnimation
initEnemyIdleAnim picBundle C.Vampire = C.CAnimation 0.25 (C.vampireIdlePics picBundle) 0 False
initEnemyIdleAnim picBundle C.Zombie  = C.CAnimation 0.25 (C.zombieIdlePics picBundle) 0 False


initPlayerAttackAnim :: C.CPictureBundle -> C.CAnimation
initPlayerAttackAnim picBundle = C.CAnimation 0.5 (C.playerAttackPics picBundle) 0 True


initPlayerIdleAnim :: C.CPictureBundle -> C.CAnimation
initPlayerIdleAnim picBundle = C.CAnimation 0.25 (C.playerIdlePics picBundle) 0 False


isAnimOver :: C.CAnimation -> Bool 
isAnimOver (C.CAnimation _ sprites index isFinite) = 
  isFinite && index == length sprites - 1


overrideFiniteAnimations :: C.System' ()
overrideFiniteAnimations = do 
  picBundle <- A.get A.global

  AS.cmapIf isAnimOver (\(C.CEnemy e) -> initEnemyIdleAnim picBundle e)
  AS.cmapIf isAnimOver (\C.CPlayer    -> initPlayerIdleAnim picBundle)


-- | Change an animation's index to point to the next sprite.
stepAnimation :: C.CAnimation -> C.CAnimation
stepAnimation a@(C.CAnimation _ sprites index _) = 
  let index' = (index + 1) `mod` length sprites
  in  a {C.index = index'}