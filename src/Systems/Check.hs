module Systems.Check (
  checkExit,
  checkGameOver
) 
where 

import Control.Monad (when)

import qualified Apecs                as A

import qualified Components           as C
import qualified Systems.Initialise   as SInit
import qualified Systems.Remove       as SRem
import qualified Visualise.Animations as Anim


checkExit :: C.System' ()
checkExit =
  A.cmapM_ $ \(C.CPlayer, posP :: C.CPosition) ->
    A.cmapM_ $ \(C.CExit, posE :: C.CPosition) -> 
      when (posP == posE) $ do
        A.modify A.global (succ :: C.CLevel -> C.CLevel)
        A.set A.global (C.CScreen C.LevelIntro)
        A.cmap $ \(C.CPlayer, config, picBundle) -> (C.startPosition config, Anim.initPlayerIdleAnim picBundle)
        SRem.removeBoard
        SInit.boardSetup
        SInit.setupScene


checkGameOver :: C.System' ()
checkGameOver = do 
  C.CFoodPoints fp <- A.get A.global 
  when (fp <= 0) (A.set A.global (C.CScreen C.GameOver))