module Systems.Attack (
  enemiesAttack,
  playerAttack
)
where 

import qualified Apecs                as A
import qualified Apecs.System         as AS
import qualified Linear               as L

import qualified Components           as C
import qualified Visualise.Animations as Anim


enemiesAttack :: C.System' ()
enemiesAttack =
  A.cmapM_ $ \(C.CPlayer, posP, fp, picBundle) ->
    AS.cmapIf 
      (isPlayerClose posP) 
      (\(C.CEnemy e, health) -> (Anim.initEnemyAttackAnim picBundle e, decreaseFoodPoints fp health))


isPlayerClose :: C.CPosition -> C.CPosition -> Bool 
isPlayerClose (C.CPosition (L.V2 xP yP)) (C.CPosition (L.V2 xE yE)) = 
  (xP == xE && abs(yP - yE) == 1)
  || (yP == yE && abs(xP - xE) == 1)


decreaseFoodPoints :: C.CFoodPoints -> C.CHealth -> C.CFoodPoints
decreaseFoodPoints (C.CFoodPoints fp) (C.CHealth _ damInfl _) = 
  C.CFoodPoints (fp - damInfl)


playerAttack :: C.System' ()
playerAttack = do 
  A.cmapM $ \C.CPlayer -> Anim.initPlayerAttackAnim <$> A.get A.global
  decreaseHP


decreaseHP :: C.System' ()
decreaseHP =
  A.cmapM_ $ \(C.CPlayer, C.CPosition posP) ->
    AS.cmapIf
      (\(C.CPosition pos) -> posP + L.V2 1 0 == pos || posP + L.V2 0 1 == pos)
      (\(h@(C.CHealth hp _ damRec), A.Not :: A.Not C.CPlayer) -> h {C.hp = hp - damRec} )