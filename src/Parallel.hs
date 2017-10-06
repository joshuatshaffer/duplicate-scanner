module Parallel (mapMPar, nthreads) where

import Data.Monoid
import Control.Concurrent.MVar
import Control.Concurrent

collectResults :: Monoid b => QSemN -> MVar b -> Either e b -> IO ()
collectResults sem mv (Right y) = do
  modifyMVar_ mv (\y' -> return $! y' <> y)
  signalQSemN sem 1
collectResults sem _ _ = signalQSemN sem 1

mapMPar :: (Foldable t, Monoid b) => (a -> IO b) -> t a -> IO b
mapMPar f xs = do
  mv <- newMVar mempty
  sem <- newQSemN 0
  mapM_ (\x -> forkFinally (f x) (collectResults sem mv)) xs
  waitQSemN sem (length xs)
  takeMVar mv

nthreads :: (Foldable t, Monoid b) => Int -> (a -> IO b) -> t a -> IO b
nthreads n f xs = do
  mv <- newMVar mempty
  sem <- newQSemN n
  mapM_ (\x -> waitQSemN sem 1 >> forkFinally (f x) (collectResults sem mv)) xs
  waitQSemN sem n
  takeMVar mv
