module Visualise.Animations (
  initEnemyAttackAnim,
  initEnemyIdleAnim,
  initPlayerAttackAnim,
  initPlayerHurtAnim,
  initPlayerIdleAnim,
  overrideFiniteAnimations,
  stepAnimation
)
where 

import qualified Apecs        as A
import qualified Apecs.System as AS

import qualified Components   as C


initEnemyAttackAnim :: C.CGraphics -> C.Enemy -> C.CAnimation
initEnemyAttackAnim graphics C.Vampire = C.CAnimation 0.5 (C.vampireAttackPics graphics) 0 True
initEnemyAttackAnim graphics C.Zombie  = C.CAnimation 0.5 (C.zombieAttackPics graphics) 0 True


initEnemyIdleAnim :: C.CGraphics -> C.Enemy -> C.CAnimation
initEnemyIdleAnim graphics C.Vampire = C.CAnimation 0.25 (C.vampireIdlePics graphics) 0 False
initEnemyIdleAnim graphics C.Zombie  = C.CAnimation 0.25 (C.zombieIdlePics graphics) 0 False


initPlayerAttackAnim :: C.CGraphics -> C.CAnimation
initPlayerAttackAnim graphics = C.CAnimation 0.5 (C.playerAttackPics graphics) 0 True


initPlayerHurtAnim :: C.CGraphics -> C.CAnimation
initPlayerHurtAnim graphics = C.CAnimation 0.5 (C.playerHurtPics graphics) 0 True


initPlayerIdleAnim :: C.CGraphics -> C.CAnimation
initPlayerIdleAnim graphics = C.CAnimation 0.25 (C.playerIdlePics graphics) 0 False


isAnimOver :: C.CAnimation -> Bool 
isAnimOver (C.CAnimation _ sprites index isFinite) = 
  isFinite && index == length sprites - 1


overrideFiniteAnimations :: C.System' ()
overrideFiniteAnimations = do 
  graphics <- A.get A.global

  AS.cmapIf isAnimOver (\(C.CEnemy e) -> initEnemyIdleAnim graphics e)
  AS.cmapIf isAnimOver (\C.CPlayer    -> initPlayerIdleAnim graphics)


-- | Change an animation's index to point to the next sprite.
stepAnimation :: C.CAnimation -> C.CAnimation
stepAnimation a@(C.CAnimation _ sprites index _) = 
  let index' = (index + 1) `mod` length sprites
  in  a {C.index = index'}