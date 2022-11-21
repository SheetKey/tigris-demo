{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- tigris
import Tigris

-- text
import Data.Text hiding (zip, take, length)

-- rhine
import FRP.Rhine
import Control.Monad.Schedule

-- base
import Control.Concurrent

-- apecs
import Apecs
import Apecs.Stores

-- criterion
import Criterion.Main

-- vector
import qualified Data.Vector as V


main :: IO ()
--main = benchMarks
--main = initAndRun "Game" gameLoop
main = do
  g <- wfc (V.fromList [tile1, tile2, tile3, tile4]) (4, 4) Nothing
  print g
--             n e s w
tile1 = Tile 1 1 2 1 1 1
tile2 = Tile 2 1 1 1 2 1
tile3 = Tile 3 2 2 1 2 1
tile4 = Tile 4 1 2 2 2 1

  

clsfLoop :: MonadIO m => ClSFS m (HoistClock IO (SystemT World m) (Millisecond 16)) () ()
clsfLoop =
  rotateTowardsMouse
  >>> setPosition
  >>> borderCollision
  >>> colliderCell
  >>> collide
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
  enemy
  set global $ Camera $ mkRect 0 0 800 600
  setReadOnly global $ TileMapSize $ V2 2000 1500
  flow $ rhineLoop world


-- KDTreeMap test
instance (Ord n, Num n) => IsPoint V2 n where
  getX (V2 x y) = x
  getY (V2 x y) = y
  flipP (V2 x y) = V2 y x
  distSqr (V2 x1 y1) (V2 x2 y2) = x * x + y * y
    where x = x1 - x2
          y = y1 - y2

mkTestList :: Int -> [((V2 Double), Int)]
mkTestList n = take n $ zip (take 1000 [V2 (x / 100) (y / 100) | x <- [1..], y <- [1..]]) [1..]

--tree2 :: KDTreeMap (V2 Double) Int
--tree2 = fromList2 list2
--
--points2 :: [(V2 Double, Int)]
--points2 = inRadius 0.5 (V2 0 0) tree2
--
--x = quickselect (length list2 `div` 2) (fst <$> list2)

benchMarks :: IO ()
benchMarks = defaultMain
             [ bgroup "fromListl'" [ bench "100" $ nf fromList $ mkTestList 100
                                   , bench "500" $ nf fromList $ mkTestList 500
                                   , bench "1,000" $ nf fromList $ mkTestList 1000
                                   , bench "10,000" $ nf fromList $ mkTestList 10000
                                   , bench "100,000" $ nf fromList $ mkTestList 100000
                                   , bench "1,000,000" $ nf fromList $ mkTestList 1000000
                                   , bench "10,000,000" $ nf fromList $ mkTestList 10000000
                                  ]
             , bgroup "fromListr" [ bench "100" $ nf fromListr $ mkTestList 100
                                  , bench "500" $ nf fromListr $ mkTestList 500
                                  , bench "1,000" $ nf fromListr $ mkTestList 1000
                                  , bench "10,000" $ nf fromListr $ mkTestList 10000
                                  , bench "100,000" $ nf fromListr $ mkTestList 100000
                                  , bench "1,000,000" $ nf fromListr $ mkTestList 1000000
                                  , bench "10,000,000" $ nf fromListr $ mkTestList 10000000
                                  ]
             , bgroup "fromList2l'" [ bench "100" $ nf fromList2 $ mkTestList 100
                                    , bench "500" $ nf fromList2 $ mkTestList 500
                                    , bench "1,000" $ nf fromList2 $ mkTestList 1000
                                    , bench "10,000" $ nf fromList2 $ mkTestList 10000
                                    , bench "100,000" $ nf fromList2 $ mkTestList 100000
                                    , bench "1,000,000" $ nf fromList2 $ mkTestList 1000000
                                    , bench "10,000,000" $ nf fromList2 $ mkTestList 10000000
                                  ]
             , bgroup "fromList2r" [ bench "100" $ nf fromList2r $ mkTestList 100
                                   , bench "500" $ nf fromList2r $ mkTestList 500
                                   , bench "1,000" $ nf fromList2r $ mkTestList 1000
                                   , bench "10,000" $ nf fromList2r $ mkTestList 10000
                                   , bench "100,000" $ nf fromList2r $ mkTestList 100000
                                   , bench "1,000,000" $ nf fromList2r $ mkTestList 1000000
                                   , bench "10,000,000" $ nf fromList2r $ mkTestList 10000000
                                  ]
--             , bgroup "inRadius"  [ bench "100" $ nf (inRadius 0.05 (V2 0 0)) $ fromList $ mkTestList 100
--                                  , bench "1000" $ nf (inRadius 0.05 (V2 0 0)) $ fromList $ mkTestList 1000
--                                  , bench "10000" $ nf (inRadius 0.05 (V2 0 0)) $ fromList $ mkTestList 10000
--                                  ]
             ]

