{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- tigris
import Tigris

-- text
import Data.Text

-- rhine
import FRP.Rhine
import Control.Monad.Schedule

-- base
import Control.Concurrent

-- apecs
import Apecs
import Apecs.Stores


main :: IO ()
main = initAndRun "Game" gameLoop

clsfLoop :: MonadIO m => ClSFS m (HoistClock IO (SystemT World m) (Millisecond 16)) () ()
clsfLoop =
  rotateTowardsMouse
  >>> normVelocity
  >>> borderCollision
  >>> normVelocity
  >>> setPosition
  >>> cameraProcess
  >>> updateDestination
  >>> incFrame


rhineLoop
  :: World
  -> RhineS IO (ParClockS IO 
                (ParClockS IO (SeqClockS IO
                                (HoistClock IO (SystemT World IO) Busy)
                                (HoistClock IO (SystemT World IO) Busy)
                              )
                              (ParClockS IO (HoistClock IO (SystemT World IO) (Millisecond 16))
                                            WindowResizeClock
                              )
                )
                (HoistClock IO (SystemT World IO) (Millisecond 16))
               )
  () ()
rhineLoop world =
  (handleEvent world ||@ (concurrentlySystem world) @||
    (clsfLoop @@ (HoistClock waitClock liftIO) ||@ (concurrentlySystem world) @||
     (cameraSizeOnWindowResize @@ WindowResizeClock)
    )
  )
  ||@ (concurrentlySystem world) @||
  (copyAll >>> present) @@ (HoistClock waitClock liftIO)

gameLoop :: World -> SystemT' IO ()
gameLoop world = do
  player
  set global $ Camera $ mkRect 0 0 800 600
  setReadOnly global $ TileMapSize $ V2 2000 1500
  flow $ rhineLoop world

