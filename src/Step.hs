module Step (
  stepper
)
where

import Control.Monad (void, when)

import qualified Apecs                as A

import qualified Components           as C
import qualified Systems.Removers     as SRem
import qualified Visualise.Animations as Anim


-- | Main stepping function combining all steppers.
-- To be used with Apecs.Gloss.play.
stepper :: Float -> C.System' ()
stepper dT = do
  incrTime dT

  Anim.overrideFiniteAnimations

  A.cmapM_ $ \anim -> 
    triggerEvery dT (C.period anim) 0.0 (A.cmap $ \anim -> Anim.stepAnimation anim)

  SRem.removeFood
  SRem.removeInnerWalls
  SRem.removeEnemies


-- | Run a system periodically.
-- Copied from https://github.com/jonascarpay/apecs/blob/master/examples/Shmup.md.
triggerEvery :: Float -> Float -> Float -> C.System' a -> C.System' ()
triggerEvery dT period phase sys = do
  C.CTime t <- A.get A.global
  let t' = t + phase
      trigger = floor (t'/period) /= floor ((t'+dT)/period)
  when trigger $ void sys


-- | Increment the time.
-- Copied from https://github.com/jonascarpay/apecs/blob/master/examples/Shmup.md.
incrTime :: Float -> C.System' ()
incrTime dT = A.modify A.global $ \(C.CTime t) -> C.CTime (t+dT)