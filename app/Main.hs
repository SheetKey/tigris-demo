{-# LANGUAGE DataKinds #-}

module Main where

import FRP.Tigris

import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  msf <- eraseDelay tigrisMain
  reactimate msf

printWord :: DlSF IO dl () ()
printWord = constM $ liftIO $ putStrLn "Hey"

printOtherWord :: DlSF IO dl () ()
printOtherWord = constM $ liftIO $ putStrLn "there"




tigrisMain :: Tigris IO (ParallelDelay IO (Millisecond 1000) (Millisecond 5000)) () ()
tigrisMain = Tigris
  (Parallel
    (Synchronous printWord)
    (Synchronous printOtherWord))
  (ParallelDelay waitDelay waitDelay concurrently)
