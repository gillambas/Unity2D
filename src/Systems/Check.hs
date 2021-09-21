module Systems.Check (
  checkExit,
  checkGameOver
) 
where 

import Control.Monad (when)

import qualified Apecs      as A

import qualified Components as C


checkExit :: C.System' ()
checkExit = return ()

checkGameOver :: C.System' ()
checkGameOver = do 
  C.CFoodPoints fp <- A.get A.global 
  when (fp <= 0) (A.set A.global (C.CScreen C.GameOver))