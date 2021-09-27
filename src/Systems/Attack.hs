module Systems.Attack (
  enemiesAttack,
  playerAttack
)
where 

import Control.Monad (when)

import qualified Apecs                as A
import qualified Apecs.System         as AS
import qualified Linear               as L

import qualified Components           as C
import qualified Visualise.Animations as Anim


enemiesAttack :: C.System' ()
enemiesAttack =
  A.cmapM_ $ \(C.CPlayer, posP, graphics, etyP) -> 
    A.cmapM_ $ \(C.CEnemy e, posE, health, etyE) ->
      when (isPlayerClose posP posE) $ do 
        A.set etyE (Anim.initEnemyAttackAnim graphics e)
        A.set etyP (Anim.initPlayerHurtAnim graphics)
        decreaseFoodPoints health


isPlayerClose :: C.CPosition -> C.CPosition -> Bool 
isPlayerClose (C.CPosition (L.V2 xP yP)) (C.CPosition (L.V2 xE yE)) = 
  (xP == xE && abs(yP - yE) == 1)
  || (yP == yE && abs(xP - xE) == 1)


decreaseFoodPoints :: C.CHealth -> C.System' ()
decreaseFoodPoints (C.CHealth _ damInfl _) = do
  A.modify A.global $ \(C.CFoodPoints fp) -> C.CFoodPoints (fp - damInfl)
  A.set A.global $ C.CPointsChange (mconcat ["-", show damInfl, "  "])


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