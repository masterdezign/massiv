{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad
import Control.Monad.ST
import Control.Scheduler
import Criterion.Main
import Data.Foldable as F
import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Bench as A
import qualified Data.Vector as VB
import qualified Data.Vector.Primitive as VP
import Prelude as P
import System.IO.Unsafe
import Data.Vector.Fusion.Util

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
      !arrP = computeAs P $ resize' (Sz $ totalElem sz) $ arrRLightIx2 DL Seq sz
  defaultMain
    [ env (pure (A.toVector arrP)) $ \(v :: VP.Vector Double) ->
        bgroup "Vector P" [
          bench "filter > 0" $ nf (VP.filter (> 0)) v
        ]
    , env (pure (A.toVector (convert arrP :: Array B Ix1 Double))) $ \(v :: VB.Vector Double) ->
        bgroup "Vector B" [
          bench "filter > 0" $ nf (VB.filter (> 0)) v
        ]
    , env (pure (A.toList arrP)) $ \xs ->
        bgroup "List" [
          bench "filter > 0" $ nf (P.filter (> 0)) xs
        ]
    , env (pure arrP) $ \ arr -> bgroup
        "Array"
        [ -- bench "filter with foldrS" $ whnf (computeAs P . filterA' (> 0)) arr
          bench "filterS" $ nf (filterS @P (> 0)) arr
        , bench "filterA (Seq)" $ nf (filterA @P (> 0)) arr
        , bench "filterA (Par)" $ nf (filterA @P (> 0)) (setComp Par arr)
        ]
    ]

-- Slow
filterA' :: Source r ix e => (e -> Bool) -> Array r ix e -> Array DL Ix1 e
filterA' f = foldrS collect A.empty
  where
    collect e !acc
      | f e = A.cons e acc
      | otherwise = acc

-- filterS :: (Mutable r Ix1 a, Source r' ix a) => (a -> Bool) -> Array r' ix a -> Array r Ix1 a
-- filterS f =
--   mapMaybeS
--     (\e ->
--        if f e
--          then Just e
--          else Nothing)
-- {-# INLINE filterS #-}

unsafeLinearIndex'
  :: (Monad m, Source r ix a) => Array r ix a -> Int -> m a
unsafeLinearIndex' arr i = return $! unsafeLinearIndex arr i
{-# INLINE unsafeLinearIndex' #-}

filterS :: (Mutable r Ix1 e, Source r' ix e) => (e -> Bool) -> Array r' ix e -> Array r Ix1 e
filterS f arr =
  runST $ do
    let totalLength = totalElem (size arr)
    marr <- unsafeNew (SafeSz totalLength)
    let writeMaybe !i !j =
          let e = unsafeLinearIndex arr i
           in e `seq`
              if f e
                then unsafeLinearWrite marr j e >> pure (j + 1)
                else pure j
        {-# INLINE writeMaybe #-}
    k <- loopM 0 (< totalLength) (+ 1) 0 writeMaybe
    unsafeLinearShrink marr (SafeSz k) >>= unsafeFreeze (getComp arr)
{-# INLINE filterS #-}


mapMaybeS :: (Mutable r Ix1 a, Source r' ix e) => (e -> Maybe a) -> Array r' ix e -> Array r Ix1 a
mapMaybeS f arr =
  runST $ do
    let totalLength = totalElem (size arr)
    marr <- unsafeNew (SafeSz totalLength)
    let comp = getComp arr
        writeMaybe !i !j =
          let a = unsafeLinearIndex arr i
          in case a `seq` f a of
            Nothing -> pure j
            Just e -> unsafeLinearWrite marr j e >> pure (j + 1)
        {-# INLINE writeMaybe #-}
    k <- loopM 0 (< totalLength) (+ 1) 0 writeMaybe
    unsafeLinearShrink marr (SafeSz k) >>= unsafeFreeze comp
{-# INLINE mapMaybeS #-}

imapMaybeA ::
     (Mutable r Ix1 a, Source r' ix e) => (ix -> e -> Maybe a) -> Array r' ix e -> Array r Ix1 a
imapMaybeA f arr =
  unsafePerformIO $ do
    let !sz = size arr
        !totalLength = totalElem sz
    marr <- unsafeNew (Sz totalLength)
    let !comp = getComp arr
        writeMaybe !i !j =
          let a = unsafeLinearIndex arr i
           in a `seq`
              case f (fromLinearIndex sz i) a of
                Nothing -> pure j
                Just e -> unsafeLinearWrite marr j e >> pure (j + 1)
        {-# INLINE writeMaybe #-}
    chunks <-
      withScheduler comp $ \scheduler ->
        splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
          loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
            scheduleWork scheduler $ do
              end <- loopM start (< (start + chunkLength)) (+ 1) start writeMaybe
              pure (start, end)
          when (slackStart < totalLength) $
            scheduleWork scheduler $ do
              end <- loopM slackStart (< totalLength) (+ 1) slackStart writeMaybe
              pure (slackStart, end)
    k <- F.foldlM (moveMisaligned marr marr) 0 chunks
    unsafeLinearShrink marr (SafeSz k) >>= unsafeFreeze comp
{-# INLINE imapMaybeA #-}


splitLinearlyWithM ::
     Monad m
  => Scheduler m (a, a)
  -> Int
  -> (Int -> m a)
     -- ^ Make the initial accumulator for each worker. Takes a loop starting index as the
     -- argument
  -> (Int -> a -> m a)
     -- ^ Action to run with the inner loop
  -> m ()
splitLinearlyWithM scheduler totalLength mkAcc0 action =
  splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
    let schedule !start !end =
          scheduleWork scheduler $ do
            acc0 <- mkAcc0 start
            acc <- loopM' start (\ i _ -> pure (i < end)) (+ 1) acc0 action
            pure (acc0, acc)
        {-# INLINE schedule #-}
    loopM_ 0 (< slackStart) (+ chunkLength) $ \start -> schedule start (start + chunkLength)
    when (slackStart < totalLength) $ schedule slackStart totalLength
{-# INLINE splitLinearlyWithM #-}

loopM' :: Monad m => Int -> (Int -> a -> m Bool) -> (Int -> Int) -> a -> (Int -> a -> m a) -> m a
loopM' !init' condition increment !initAcc f = go init' initAcc
  where
    go !step !acc = do
      shouldContinue <- condition step acc
      if shouldContinue
        then f step acc >>= go (increment step)
        else pure acc
{-# INLINE loopM' #-}


mapMaybeA ::
     (Mutable r Ix1 a, Source r' ix e) => (e -> Maybe a) -> Array r' ix e -> Array r Ix1 a
mapMaybeA f = imapMaybeA (const f)
{-# INLINE mapMaybeA #-}


ifilterA ::
     (Mutable r Ix1 e, Source r' ix e) => (ix -> e -> Bool) -> Array r' ix e -> Array r Ix1 e
ifilterA f arr =
  unsafePerformIO $ do
    let !sz = size arr
        !totalLength = totalElem sz
    marr <- unsafeNew (Sz totalLength)
    let !comp = getComp arr
        writeMaybe !i !j =
          let e = unsafeLinearIndex arr i
           in e `seq`
              if f (fromLinearIndex sz i) e
                then unsafeLinearWrite marr j e >> pure (j + 1)
                else pure j
        {-# INLINE writeMaybe #-}
    chunks <-
      withScheduler comp $ \scheduler ->
        splitLinearlyWithM scheduler totalLength pure writeMaybe
    k <- F.foldlM (moveMisaligned marr marr) 0 chunks
    unsafeLinearShrink marr (SafeSz k) >>= unsafeFreeze comp
{-# INLINE ifilterA #-}

filterA
  :: (Mutable r Ix1 a, Source r' ix a) =>
     (a -> Bool) -> Array r' ix a -> Array r Ix1 a
filterA f = ifilterA (const f)
{-# INLINE filterA #-}

-- ifilterA :: (Mutable r Ix1 a, Source r' ix a) => (ix -> a -> Bool) -> Array r' ix a -> Array r Ix1 a
-- ifilterA f =
--   imapMaybeA
--     (\ix e ->
--        if f ix e
--          then Just e
--          else Nothing)
-- {-# INLINE ifilterA #-}



moveMisaligned ::
     (PrimMonad m, Mutable r ix e)
  => MArray (PrimState m) r ix e
  -> MArray (PrimState m) r ix e
  -> Ix1
  -> (Ix1, Ix1)
  -> m Ix1
moveMisaligned marrs marrd prevEnd (start, end) = do
  let !len = end - start
  unless (prevEnd == start) $
    unsafeLinearCopy marrs start marrd prevEnd (SafeSz len)
  pure $! prevEnd + len
{-# INLINE moveMisaligned #-}



unsafeLinearMove ::
     (Mutable r ix' e, Mutable r ix e, PrimMonad m)
  => MArray (PrimState m) r ix' e -- ^ Source mutable array
  -> Ix1 -- ^ Starting index at source array
  -> MArray (PrimState m) r ix e -- ^ Target mutable array
  -> Ix1 -- ^ Starting index at target array
  -> Sz1 -- ^ Number of elements to copy
  -> m ()
unsafeLinearMove src isrc dst idst (Sz k) =
  let delta = idst - isrc
   in loopM_ isrc (< k + isrc) (+ 1) $ \i ->
        unsafeLinearRead src i >>= unsafeLinearWrite dst (i + delta)
{-# INLINE unsafeLinearMove #-}
