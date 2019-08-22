{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad
import Control.Monad.ST
import Control.Scheduler
import Criterion.Main
import Data.Foldable as F
import Data.Massiv.Array as A
import Data.Massiv.Array.Delayed.Stream as A hiding (moveMisaligned,
                                              unsafeLinearMove)
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Bench as A
import Data.Massiv.Core
import qualified Data.Vector.Primitive as VP
import Prelude as P
import System.IO.Unsafe

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
      !arr = computeAs P $ resize' (Sz $ totalElem sz) $ arrRLightIx2 DL Seq sz
      !v = A.toVector arr :: VP.Vector Double
      !arrs = toStream arr
  defaultMain
    [ bgroup "Vector" [
        bench "filter > 0" $ whnf (VP.filter (> 0)) v
        , bench "Mutable (Seq)" $ whnf (filterA'' @P (> 0)) arr
        ]
    , bgroup
        "Array"
        [ bench "filter with foldrS" $ whnf (computeAs P . filterA' (> 0)) arr
        , bench "Mutable (Seq)" $ whnf (filterA @P (> 0)) arr
        , bench "Mutable (Par)" $ whnf (filterA @P (> 0)) (setComp Par arr)
        , bench "DS (Seq) (no filter)" $ whnf (computeS @P) arrs
        , bench "DS (Seq)" $ whnf (computeS @P . A.filter (> 0)) arrs
        , bench "DS (Par)" $ whnf (computeAs P . A.filter (> 0)) (toStream (setComp Par arr))
        ]
    ]

-- Slow
filterA' :: Source r ix e => (e -> Bool) -> Array r ix e -> Array DL Ix1 e
filterA' f = foldrS collect A.empty
  where
    collect e !acc
      | f e = A.cons e acc
      | otherwise = acc

filterA''
  :: (Mutable r Ix1 a, Source r' ix a) =>
     (a -> Bool) -> Array r' ix a -> Array r Ix1 a
filterA'' f =
  mapMaybeA'
    (\e ->
       if f e
         then Just e
         else Nothing)
{-# INLINE filterA'' #-}

mapMaybeA' :: (Mutable r Ix1 a, Source r' ix e) => (e -> Maybe a) -> Array r' ix e -> Array r Ix1 a
mapMaybeA' f arr =
  runST $ do
    let totalLength = totalElem (size arr)
    marr <- unsafeNew (Sz totalLength)
    let comp = getComp arr
        writeMaybe !j !e =
          case f e of
            Nothing -> pure j
            Just a -> unsafeLinearWrite marr j a >> pure (j + 1)
        {-# INLINE writeMaybe #-}
    k <- A.foldlM writeMaybe 0 arr
    unsafeFreeze comp $ unsafeMutableSlice 0 (SafeSz k) marr

mapMaybeA :: (Mutable r Ix1 a, Source r' ix e) => (e -> Maybe a) -> Array r' ix e -> Array r Ix1 a
mapMaybeA f arr =
  unsafePerformIO $ do
    let totalLength = totalElem (size arr)
    marr <- unsafeNew (Sz totalLength)
    let comp = getComp arr
        writeMaybe !i !j =
          case f (unsafeLinearIndex arr i) of
            Nothing -> pure j
            Just e  -> unsafeLinearWrite marr j e >> pure (j + 1)
        {-# INLINE writeMaybe #-}
    chunks <- withScheduler comp $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork scheduler $ do
            end <- loopM start (< (start + chunkLength)) (+ 1) start writeMaybe
            pure (start, end)
        when (slackStart < totalLength) $
          scheduleWork scheduler $ do
            end <- loopM slackStart (< totalLength) (+ 1) slackStart writeMaybe
            pure (slackStart, end)
    marr' <- unsafeNew (Sz (F.foldl' (\acc (start, end) -> acc + end - start) 0 chunks))
    _k <- F.foldlM (moveMisaligned marr marr') 0 chunks
    unsafeFreeze comp marr'
{-# INLINE mapMaybeA #-}


filterA
  :: (Mutable r Ix1 a, Source r' ix a) =>
     (a -> Bool) -> Array r' ix a -> Array r Ix1 a
filterA f =
  mapMaybeA
    (\e ->
       if f e
         then Just e
         else Nothing)
{-# INLINE filterA #-}



-- unsafeLinearMove ::
--      (Mutable r ix' e, Mutable r ix e, PrimMonad m)
--   => MArray (PrimState m) r ix' e -- ^ Source mutable array
--   -> Ix1 -- ^ Starting index at source array
--   -> MArray (PrimState m) r ix e -- ^ Target mutable array
--   -> Ix1 -- ^ Starting index at target array
--   -> Sz1 -- ^ Number of elements to copy
--   -> m ()
-- unsafeLinearMove src isrc dst idst (Sz k) =
--   let delta = idst - isrc
--    in loopM_ isrc (< k + isrc) (+ 1) $ \i ->
--         unsafeLinearRead src i >>= unsafeLinearWrite dst (i + delta)
-- {-# INLINE unsafeLinearMove #-}

moveMisaligned ::
     (PrimMonad m, Mutable r ix e)
  => MArray (PrimState m) r ix e
  -> MArray (PrimState m) r ix e
  -> Ix1
  -> (Ix1, Ix1)
  -> m Ix1
moveMisaligned marrs marrd prevEnd (start, end) = do
  let !len = end - start
  --unless (prevEnd == start) $
  unsafeLinearCopy marrs start marrd prevEnd (SafeSz len)
  pure $! prevEnd + len
{-# INLINE moveMisaligned #-}
