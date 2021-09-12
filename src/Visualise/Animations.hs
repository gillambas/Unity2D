module Visualise.Animations (
  stepAnimation
)
where 

import qualified Components as C


-- | Step an animation's index to point to the next sprite.
stepAnimation :: C.CAnimation -> C.CAnimation
stepAnimation a@(C.CAnimation _ sprites index) = 
  let index' = (index + 1) `mod` length sprites
  in  a {C.index = index'} 
