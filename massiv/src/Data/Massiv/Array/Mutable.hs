{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Massiv.Array.Mutable
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Mutable
  ( -- ** Size
    msize
    -- ** Element-wise mutation
  , read
  , readM
  , read'
  , write
  , writeM
  , write'
  , modify
  , modifyM
  , modifyM_
  , modify'
  , swap
  , swapM
  , swapM_
  , swap'
  -- ** Operations on @MArray@
  -- *** Immutable conversion
  , new
  , thaw
  , thawS
  , freeze
  , freezeS
  -- *** Create mutable
  , makeMArray
  , makeMArrayLinear
  , makeMArrayS
  , makeMArrayLinearS
  -- *** Create pure
  , createArray_
  , createArray
  , createArrayS_
  , createArrayS
  , createArrayST_
  , createArrayST
  -- *** Generate
  , generateArray
  , generateArrayLinear
  , generateArrayS
  , generateArrayLinearS
  -- *** Stateful worker threads
  , generateArrayWS
  , generateArrayLinearWS
  -- *** Unfold
  , unfoldrPrimM_
  , iunfoldrPrimM_
  , unfoldrPrimM
  , iunfoldrPrimM
  , unfoldlPrimM_
  , iunfoldlPrimM_
  , unfoldlPrimM
  , iunfoldlPrimM
  -- *** Mapping
  , forPrimM
  , forPrimM_
  , iforPrimM
  , iforPrimM_
  , iforLinearPrimM
  , iforLinearPrimM_
  -- *** Modify
  , withMArray
  , withMArrayS
  , withMArrayST
  -- *** Initialize
  , initialize
  , initializeNew
  -- ** Computation
  , Mutable
  , MArray
  , RealWorld
  , computeInto
  , loadArray
  , loadArrayS
  ) where

-- TODO: add fromListM, et al.

import Control.Monad (void, when, unless, (>=>))
import Control.Monad.ST
import Control.Scheduler
import Data.Massiv.Core.Common
import Prelude hiding (mapM, read)

-- | /O(n)/ - Initialize a new mutable array. All elements will be set to some default value. For
-- boxed arrays in will be a thunk with `Uninitialized` exception, while for others it will be
-- simply zeros.
--
-- ==== __Examples__
--
-- >>> import Data.Massiv.Array
-- >>> marr <- new (Sz2 2 6) :: IO (MArray RealWorld P Ix2 Int)
-- >>> freeze Seq marr
-- Array P Seq (Sz (2 :. 6))
--   [ [ 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
--
-- Or using @TypeApplications@:
--
-- >>> :set -XTypeApplications
-- >>> new @P @Ix2 @Int (Sz2 2 6) >>= freezeS
-- Array P Seq (Sz (2 :. 6))
--   [ [ 0, 0, 0, 0, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
-- >>> new @B @_ @Int (Sz2 2 6) >>= (`readM` 1)
-- *** Exception: Uninitialized
--
-- @since 0.1.0
new ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Sz ix
  -> m (MArray (PrimState m) r ix e)
new = initializeNew Nothing
{-# INLINE new #-}

-- | /O(n)/ - Make a mutable copy of a pure array. Keep in mind that both `freeze` and `thaw` trigger a
-- copy of the full array.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XTypeApplications
-- >>> arr <- fromListsM @U @Ix2 @Double Par [[12,21],[13,31]]
-- >>> marr <- thaw arr
-- >>> modify marr (pure . (+ 10)) (1 :. 0)
-- Just 13.0
-- >>> freeze Par marr
-- Array U Par (Sz (2 :. 2))
--   [ [ 12.0, 21.0 ]
--   , [ 23.0, 31.0 ]
--   ]
--
-- @since 0.1.0
thaw :: forall r ix e m. (Mutable r ix e, MonadIO m) => Array r ix e -> m (MArray RealWorld r ix e)
thaw arr =
  liftIO $ do
    let sz = size arr
        totalLength = totalElem sz
    marr <- unsafeNew sz
    withScheduler_ (getComp arr) $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork_ scheduler $ unsafeArrayLinearCopy arr start marr start (SafeSz chunkLength)
        let slackLength = totalLength - slackStart
        when (slackLength > 0) $
          scheduleWork_ scheduler $
          unsafeArrayLinearCopy arr slackStart marr slackStart (SafeSz slackLength)
    pure marr
{-# INLINE thaw #-}

-- | Same as `thaw`, but restrict computation to sequential only.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> :set -XOverloadedLists
-- >>> thawS @P @Ix1 @Double [1..10]
-- >>> marr <- thawS @P @Ix1 @Double [1..10]
-- >>> writeM marr 5 100
-- >>> freezeS marr
-- Array P Seq (Sz1 10)
--   [ 1.0, 2.0, 3.0, 4.0, 5.0, 100.0, 7.0, 8.0, 9.0, 10.0 ]
--
-- @since 0.3.0
thawS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Array r ix e
  -> m (MArray (PrimState m) r ix e)
thawS arr = do
  tmarr <- unsafeNew (size arr)
  unsafeArrayLinearCopy arr 0 tmarr 0 (SafeSz (totalElem (size arr)))
  pure tmarr
{-# INLINE thawS #-}


-- | /O(n)/ - Yield an immutable copy of the mutable array. Note that mutable representations
-- have to be the same.
--
-- ==== __Example__
--
-- >>> import Data.Massiv.Array
-- >>> marr <- new @P @_ @Int (Sz2 2 6)
-- >>> forM_ (range Seq 0 (Ix2 1 4)) $ \ix -> write marr ix 9
-- >>> freeze Seq marr
-- Array P Seq (Sz (2 :. 6))
--   [ [ 9, 9, 9, 9, 0, 0 ]
--   , [ 0, 0, 0, 0, 0, 0 ]
--   ]
--
-- @since 0.1.0
freeze ::
     forall r ix e m. (Mutable r ix e, MonadIO m)
  => Comp
  -> MArray RealWorld r ix e
  -> m (Array r ix e)
freeze comp smarr =
  liftIO $ do
    let sz = msize smarr
        totalLength = totalElem sz
    tmarr <- unsafeNew sz
    withScheduler_ comp $ \scheduler ->
      splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
        loopM_ 0 (< slackStart) (+ chunkLength) $ \ !start ->
          scheduleWork_ scheduler $ unsafeLinearCopy smarr start tmarr start (SafeSz chunkLength)
        let slackLength = totalLength - slackStart
        when (slackLength > 0) $
          scheduleWork_ scheduler $
          unsafeLinearCopy smarr slackStart tmarr slackStart (SafeSz slackLength)
    unsafeFreeze comp tmarr
{-# INLINE freeze #-}


-- | Same as `freeze`, but do the copy of supplied muable array sequentially. Also, unlike `freeze`
-- that has to be done in `IO`, `freezeS` can be used with `ST`.
--
-- @since 0.3.0
freezeS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => MArray (PrimState m) r ix e
  -> m (Array r ix e)
freezeS smarr = do
  let sz = msize smarr
  tmarr <- unsafeNew sz
  unsafeLinearCopy smarr 0 tmarr 0 (SafeSz (totalElem sz))
  unsafeFreeze Seq tmarr
{-# INLINE freezeS #-}


newMaybeInitialized ::
     (Load r' ix e, Mutable r ix e, PrimMonad m) => Array r' ix e -> m (MArray (PrimState m) r ix e)
newMaybeInitialized !arr = initializeNew (defaultElement arr) (size arr)
{-# INLINE newMaybeInitialized #-}


-- | Load sequentially a pure array into the newly created mutable array.
--
-- @since 0.3.0
loadArrayS ::
     forall r ix e r' m. (Load r' ix e, Mutable r ix e, PrimMonad m)
  => Array r' ix e
  -> m (MArray (PrimState m) r ix e)
loadArrayS arr = do
  marr <- newMaybeInitialized arr
  unsafeLoadIntoS marr arr
  -- loadArrayM trivialScheduler_ arr (unsafeLinearWrite marr)
  -- pure marr
{-# INLINE loadArrayS #-}


-- | Load a pure array into the newly created mutable array, while respecting computation startegy.
--
-- @since 0.3.0
loadArray ::
     forall r ix e r' m. (Load r' ix e, Mutable r ix e, MonadIO m)
  => Array r' ix e
  -> m (MArray RealWorld r ix e)
loadArray arr =
  liftIO $ do
    marr <- newMaybeInitialized arr
    unsafeLoadInto marr arr
    -- withScheduler_ (getComp arr) $ \scheduler -> loadArrayM scheduler arr (unsafeLinearWrite marr)
    -- pure marr
{-# INLINE loadArray #-}



-- | Compute an Array while loading the results into the supplied mutable target array. Number of
-- elements for arrays must agree, otherwise `SizeElementsMismatchException` exception is thrown.
--
-- @since 0.1.3
computeInto ::
     (Load r' ix' e, Mutable r ix e, MonadIO m)
  => MArray RealWorld r ix e -- ^ Target Array
  -> Array r' ix' e -- ^ Array to load
  -> m ()
computeInto !mArr !arr =
  liftIO $ do
    unless (totalElem (msize mArr) == totalElem (size arr)) $
      throwM $ SizeElementsMismatchException (msize mArr) (size arr)
    withScheduler_ (getComp arr) $ \scheduler -> loadArrayM scheduler arr (unsafeLinearWrite mArr)
{-# INLINE computeInto #-}


-- | Create a mutable array using an index aware generating action.
--
-- @since 0.3.0
makeMArrayS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Sz ix -- ^ Size of the create array
  -> (ix -> m e) -- ^ Element generating action
  -> m (MArray (PrimState m) r ix e)
makeMArrayS sz f = makeMArrayLinearS sz (f . fromLinearIndex sz)
{-# INLINE makeMArrayS #-}


-- | Same as `makeMArrayS`, but index supplied to the action is row-major linear index.
--
-- @since 0.3.0
makeMArrayLinearS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Sz ix
  -> (Int -> m e)
  -> m (MArray (PrimState m) r ix e)
makeMArrayLinearS sz f = do
  marr <- unsafeNew sz
  loopM_ 0 (< totalElem (msize marr)) (+ 1) (\ !i -> f i >>= unsafeLinearWrite marr i)
  return marr
{-# INLINE makeMArrayLinearS #-}

-- | Just like `makeMArrayS`, but also accepts computation strategy and runs in `IO`.
--
-- @since 0.3.0
makeMArray ::
     forall r ix e m. (PrimMonad m, MonadUnliftIO m, Mutable r ix e)
  => Comp
  -> Sz ix
  -> (ix -> m e)
  -> m (MArray (PrimState m) r ix e)
makeMArray comp sz f = makeMArrayLinear comp sz (f . fromLinearIndex sz)
{-# INLINE makeMArray #-}


-- | Just like `makeMArrayLinearS`, but also accepts computation strategy and runs in `IO`.
--
-- @since 0.3.0
makeMArrayLinear ::
     forall r ix e m. (PrimMonad m, MonadUnliftIO m, Mutable r ix e)
  => Comp
  -> Sz ix
  -> (Int -> m e)
  -> m (MArray (PrimState m) r ix e)
makeMArrayLinear comp sz f = do
  marr <- unsafeNew sz
  withScheduler_ comp $ \scheduler ->
    splitLinearlyWithM_ scheduler (totalElem sz) f (unsafeLinearWrite marr)
  return marr
{-# INLINE makeMArrayLinear #-}




-- | Create a new array by supplying an action that will fill the new blank mutable array. Use
-- `createArray` if you'd like to keep the result of the filling function.
--
-- ====__Examples__
--
-- >>> :set -XTypeApplications
-- >>> import Data.Massiv.Array
-- >>> createArray_ @P @_ @Int Seq (Sz1 2) (\ s marr -> scheduleWork s (writeM marr 0 10) >> scheduleWork s (writeM marr 1 11))
-- Array P Seq (Sz1 2)
--   [ 10, 11 ]
--
-- @since 0.3.0
--
createArray_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m, MonadUnliftIO m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (Scheduler m () -> MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (Array r ix e)
createArray_ comp sz action = do
  marr <- new sz
  withScheduler_ comp (`action` marr)
  unsafeFreeze comp marr
{-# INLINE createArray_ #-}

-- | Just like `createArray_`, but together with `Array` it returns results of scheduled filling
-- actions.
--
-- @since 0.3.0
--
createArray ::
     forall r ix e a m b. (Mutable r ix e, PrimMonad m, MonadUnliftIO m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (Scheduler m a -> MArray (PrimState m) r ix e -> m b)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m ([a], Array r ix e)
createArray comp sz action = do
  marr <- new sz
  a <- withScheduler comp (`action` marr)
  arr <- unsafeFreeze comp marr
  return (a, arr)
{-# INLINE createArray #-}


-- | Create a new array by supplying an action that will fill the new blank mutable array. Use
-- `createArrayS` if you'd like to keep the result of the filling function.
--
-- ====__Examples__
--
-- >>> :set -XTypeApplications
-- >>> import Data.Massiv.Array
-- >>> createArrayS_ @P @_ @Int Seq (Sz1 2) (\ marr -> write marr 0 10 >> write marr 1 12)
-- Array P Seq (Sz1 2)
--   [ 10, 12 ]
--
-- @since 0.3.0
--
createArrayS_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (Array r ix e)
createArrayS_ comp sz action = snd <$> createArrayS comp sz action
{-# INLINE createArrayS_ #-}

-- | Just like `createArray_`, but together with `Array` it returns the result of the filling action.
--
-- @since 0.3.0
--
createArrayS ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy to use after `MArray` gets frozen and onward.
  -> Sz ix -- ^ Size of the newly created array
  -> (MArray (PrimState m) r ix e -> m a)
  -- ^ An action that should fill all elements of the brand new mutable array
  -> m (a, Array r ix e)
createArrayS comp sz action = do
  marr <- new sz
  a <- action marr
  arr <- unsafeFreeze comp marr
  return (a, arr)
{-# INLINE createArrayS #-}

-- | Just like `createArrayS_`, but restricted to `ST`.
--
-- @since 0.3.0
--
createArrayST_ ::
     forall r ix e a. Mutable r ix e
  => Comp
  -> Sz ix
  -> (forall s. MArray s r ix e -> ST s a)
  -> Array r ix e
createArrayST_ comp sz action = runST $ createArrayS_ comp sz action
{-# INLINE createArrayST_ #-}


-- | Just like `createArrayS`, but restricted to `ST`.
--
-- @since 0.2.6
--
createArrayST ::
     forall r ix e a. Mutable r ix e
  => Comp
  -> Sz ix
  -> (forall s. MArray s r ix e -> ST s a)
  -> (a, Array r ix e)
createArrayST comp sz action = runST $ createArrayS comp sz action
{-# INLINE createArrayST #-}


-- | Sequentially generate a pure array. Much like `makeArray` creates a pure array this
-- function will use `Mutable` interface to generate a pure `Array` in the end, except that
-- computation strategy is set to `Seq`. Element producing function no longer has to be pure
-- but is a stateful action, becuase it is restricted to `PrimMonad` thus allows for sharing
-- the state between computation of each element.
--
-- @since 0.2.6
--
-- ====__Examples__
--
-- >>> import Data.Massiv.Array
-- >>> import Data.IORef
-- >>> ref <- newIORef (0 :: Int)
-- >>> generateArrayS (Sz1 6) (\ i -> modifyIORef' ref (+i) >> print i >> pure i) :: IO (Array U Ix1 Int)
-- 0
-- 1
-- 2
-- 3
-- 4
-- 5
-- Array U Seq (Sz1 6)
--   [ 0, 1, 2, 3, 4, 5 ]
-- >>> readIORef ref
-- 15
--
generateArrayS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Sz ix -- ^ Resulting size of the array
  -> (ix -> m e) -- ^ Element producing generator
  -> m (Array r ix e)
generateArrayS sz gen = generateArrayLinearS sz (gen . fromLinearIndex sz)
{-# INLINE generateArrayS #-}

-- | Same as `generateArray` but with action that accepts row-major linear index.
--
-- @since 0.3.0
generateArrayLinearS ::
     forall r ix e m. (Mutable r ix e, PrimMonad m)
  => Sz ix -- ^ Resulting size of the array
  -> (Int -> m e) -- ^ Element producing generator
  -> m (Array r ix e)
generateArrayLinearS sz gen = do
  marr <- unsafeNew sz
  loopM_ 0 (< totalElem (msize marr)) (+ 1) $ \i -> gen i >>= unsafeLinearWrite marr i
  unsafeFreeze Seq marr
{-# INLINE generateArrayLinearS #-}


-- | Just like `generateArrayS`, except this generator __will__ respect the supplied computation
-- strategy, and for that reason it is restricted to `IO`.
--
-- @since 0.2.6
generateArray ::
     forall r ix e m. (MonadUnliftIO m, PrimMonad m, Mutable r ix e)
  => Comp
  -> Sz ix
  -> (ix -> m e)
  -> m (Array r ix e)
generateArray comp sz f = generateArrayLinear comp sz (f . fromLinearIndex sz)
{-# INLINE generateArray #-}

-- | Just like `generateArrayIO`, but action supplied will receive a row-major linear index.
--
-- @since 0.3.0
generateArrayLinear ::
     forall r ix e m. (MonadUnliftIO m, PrimMonad m, Mutable r ix e)
  => Comp
  -> Sz ix
  -> (Int -> m e)
  -> m (Array r ix e)
generateArrayLinear comp sz f = makeMArrayLinear comp sz f >>= unsafeFreeze comp
{-# INLINE generateArrayLinear #-}


-- | Same as `generateArrayWS`, but use linear indexing instead.
--
-- @since 0.3.4
generateArrayLinearWS ::
     forall r ix e s m. (Mutable r ix e, MonadUnliftIO m, PrimMonad m)
  => WorkerStates s
  -> Sz ix
  -> (Int -> s -> m e)
  -> m (Array r ix e)
generateArrayLinearWS states sz make = do
  marr <- unsafeNew sz
  withSchedulerWS_ states $ \schedulerWS ->
    splitLinearlyWithStatefulM_
      schedulerWS
      (totalElem sz)
      make
      (unsafeLinearWrite marr)
  unsafeFreeze (workerStatesComp states) marr
{-# INLINE generateArrayLinearWS #-}

-- | Use per worker thread state while generating elements of the array. Very useful for
-- things that are not thread safe.
--
-- @since 0.3.4
generateArrayWS ::
     forall r ix e s m. (Mutable r ix e, MonadUnliftIO m, PrimMonad m)
  => WorkerStates s
  -> Sz ix
  -> (ix -> s -> m e)
  -> m (Array r ix e)
generateArrayWS states sz make = generateArrayLinearWS states sz (make . fromLinearIndex sz)
{-# INLINE generateArrayWS #-}


-- | Sequentially unfold an array from the left.
--
-- ====__Examples__
--
-- Create an array with Fibonacci numbers while performing and `IO` action on the accumulator for
-- each element of the array.
--
-- >>> import Data.Massiv.Array
-- >>> unfoldrPrimM_ Seq  (Sz1 10) (\a@(f0, f1) -> let fn = f0 + f1 in print a >> return (f0, (f1, fn))) (0, 1) :: IO (Array P Ix1 Int)
-- (0,1)
-- (1,1)
-- (1,2)
-- (2,3)
-- (3,5)
-- (5,8)
-- (8,13)
-- (13,21)
-- (21,34)
-- (34,55)
-- Array P Seq (Sz1 10)
--   [ 0, 1, 1, 2, 3, 5, 8, 13, 21, 34 ]
--
-- @since 0.3.0
--
unfoldrPrimM_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> m (e, a)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (Array r ix e)
unfoldrPrimM_ comp sz gen acc0 = snd <$> unfoldrPrimM comp sz gen acc0
{-# INLINE unfoldrPrimM_ #-}

-- | Same as `unfoldrPrimM_` but do the unfolding with index aware function.
--
-- @since 0.3.0
--
iunfoldrPrimM_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> ix -> m (e, a)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (Array r ix e)
iunfoldrPrimM_ comp sz gen acc0 = snd <$> iunfoldrPrimM comp sz gen acc0
{-# INLINE iunfoldrPrimM_ #-}


-- | Just like `iunfoldrPrimM_`, but also returns the final value of the accumulator.
--
-- @since 0.3.0
iunfoldrPrimM ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> ix -> m (e, a)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (a, Array r ix e)
iunfoldrPrimM comp sz gen acc0 =
  createArrayS comp sz $ \marr ->
    let sz' = msize marr
     in iterLinearM sz' 0 (totalElem sz') 1 (<) acc0 $ \i ix acc -> do
          (e, acc') <- gen acc ix
          unsafeLinearWrite marr i e
          pure $! acc'
{-# INLINE iunfoldrPrimM #-}

-- | Just like `iunfoldrPrimM`, but do the unfolding with index aware function.
--
-- @since 0.3.0
unfoldrPrimM ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> m (e, a)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (a, Array r ix e)
unfoldrPrimM comp sz gen acc0 =
  createArrayS comp sz $ \marr ->
    let sz' = msize marr
     in loopM 0 (< totalElem sz') (+1) acc0 $ \i acc -> do
          (e, acc') <- gen acc
          unsafeLinearWrite marr i e
          pure $! acc'
{-# INLINE unfoldrPrimM #-}

-- | Sequentially unfold an array from the left.
--
-- ====__Examples__
--
-- Create an array with Fibonacci numbers starting at the end while performing and `IO` action on
-- the accumulator for each element of the array.
--
-- >>> import Data.Massiv.Array
-- >>> unfoldlPrimM_ Seq  (Sz1 10) (\a@(f0, f1) -> let fn = f0 + f1 in print a >> return ((f1, fn), f0)) (0, 1) :: IO (Array P Ix1 Int)
-- (0,1)
-- (1,1)
-- (1,2)
-- (2,3)
-- (3,5)
-- (5,8)
-- (8,13)
-- (13,21)
-- (21,34)
-- (34,55)
-- Array P Seq (Sz1 10)
--   [ 34, 21, 13, 8, 5, 3, 2, 1, 1, 0 ]
--
-- @since 0.3.0
--
unfoldlPrimM_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (Array r ix e)
unfoldlPrimM_ comp sz gen acc0 = snd <$> unfoldlPrimM comp sz gen acc0
{-# INLINE unfoldlPrimM_ #-}

-- | Same as `unfoldlPrimM_` but do the unfolding with index aware function.
--
-- @since 0.3.0
--
iunfoldlPrimM_ ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> ix -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (Array r ix e)
iunfoldlPrimM_ comp sz gen acc0 = snd <$> iunfoldlPrimM comp sz gen acc0
{-# INLINE iunfoldlPrimM_ #-}


-- | Just like `iunfoldlPrimM_`, but also returns the final value of the accumulator.
--
-- @since 0.3.0
iunfoldlPrimM ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> ix -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (a, Array r ix e)
iunfoldlPrimM comp sz gen acc0 =
  createArrayS comp sz $ \marr ->
    let sz' = msize marr
     in iterLinearM sz' (totalElem sz' - 1) 0 (negate 1) (>=) acc0 $ \i ix acc -> do
          (acc', e) <- gen acc ix
          unsafeLinearWrite marr i e
          pure $! acc'
{-# INLINE iunfoldlPrimM #-}

-- | Just like `iunfoldlPrimM`, but do the unfolding with index aware function.
--
-- @since 0.3.0
unfoldlPrimM ::
     forall r ix e a m. (Mutable r ix e, PrimMonad m)
  => Comp -- ^ Computation strategy (ignored during initial creation)
  -> Sz ix -- ^ Size of the desired array
  -> (a -> m (a, e)) -- ^ Unfolding action
  -> a -- ^ Initial accumulator
  -> m (a, Array r ix e)
unfoldlPrimM comp sz gen acc0 =
  createArrayS comp sz $ \marr ->
    let sz' = msize marr
     in loopDeepM 0 (< totalElem sz') (+1) acc0 $ \i acc -> do
          (acc', e) <- gen acc
          unsafeLinearWrite marr i e
          pure $! acc'
{-# INLINE unfoldlPrimM #-}

-- | Sequentially loop over a mutable array while reading each element and applying an
-- action to it. There is no mutation to the array, unless the action itself modifies it.
--
-- @since 0.4.0
forPrimM_ :: (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (e -> m ()) -> m ()
forPrimM_ marr f =
  loopM_ 0 (< totalElem (msize marr)) (+1) (unsafeLinearRead marr >=> f)
{-# INLINE forPrimM_ #-}

-- | Sequentially loop over a mutable array while modifying each element with an action.
--
-- @since 0.4.0
forPrimM :: (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (e -> m e) -> m ()
forPrimM marr f =
  loopM_ 0 (< totalElem (msize marr)) (+1) (unsafeLinearModify marr f)
{-# INLINE forPrimM #-}


-- | Sequentially loop over a mutable array while reading each element and applying an
-- index aware action to it. There is no mutation to the array, unless the
-- action itself modifies it.
--
-- @since 0.4.0
iforPrimM_ ::
     (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (ix -> e -> m ()) -> m ()
iforPrimM_ marr f = iforLinearPrimM_ marr (f . fromLinearIndex (msize marr))
{-# INLINE iforPrimM_ #-}

-- | Sequentially loop over a mutable array while modifying each element with an index aware action.
--
-- @since 0.4.0
iforPrimM ::
     (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (ix -> e -> m e) -> m ()
iforPrimM marr f = iforLinearPrimM marr (f . fromLinearIndex (msize marr))
{-# INLINE iforPrimM #-}


-- | Sequentially loop over a mutable array while reading each element and applying a
-- linear index aware action to it. There is no mutation to the array, unless the action
-- itself modifies it.
--
-- @since 0.4.0
iforLinearPrimM_ ::
     (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (Int -> e -> m ()) -> m ()
iforLinearPrimM_ marr f =
  loopM_ 0 (< totalElem (msize marr)) (+ 1) (\i -> unsafeLinearRead marr i >>= f i)
{-# INLINE iforLinearPrimM_ #-}

-- | Sequentially loop over a mutable array while modifying each element with an index aware action.
--
-- @since 0.4.0
iforLinearPrimM ::
     (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> (Int -> e -> m e) -> m ()
iforLinearPrimM marr f =
  loopM_ 0 (< totalElem (msize marr)) (+ 1) (\i -> unsafeLinearModify marr (f i) i)
{-# INLINE iforLinearPrimM #-}

-- | Create a copy of a pure array, mutate it in place and return its frozen version. The big
-- difference between `withMArrayS` is that it's not only gonna respect the computation strategy
-- supplied to it while making a copy, but it will also pass extra argumens to the action that
-- suppose to modify the mutable copy of the source array. These two extra arguments are:
--
-- * Number of capabilities derived from the `Comp`utation strategy of the array.
--
-- * An action that can be used to schedule arbitrary number of jobs that will be executed in
--   parallel.
--
-- * And, of course, the mutable array itself.
--
-- @since 0.3.0
withMArray ::
     (Mutable r ix e, MonadUnliftIO m)
  => Array r ix e
  -> (Scheduler m () -> MArray RealWorld r ix e -> m a)
  -> m (Array r ix e)
withMArray arr action = do
  marr <- thaw arr
  withScheduler_ (getComp arr) (`action` marr)
  liftIO $ unsafeFreeze (getComp arr) marr
{-# INLINE withMArray #-}


-- | Create a copy of a pure array, mutate it in place and return its frozen version. The important
-- benefit over doing a manual `thawS` followed by a `freezeS` is that an array will be only copied
-- once.
--
-- @since 0.3.2
withMArrayS ::
     (Mutable r ix e, PrimMonad m)
  => Array r ix e
  -> (MArray (PrimState m) r ix e -> m a)
  -> m (Array r ix e)
withMArrayS arr action = do
  marr <- thawS arr
  _ <- action marr
  unsafeFreeze (getComp arr) marr
{-# INLINE withMArrayS #-}


-- | Same as `withMArrayS` but in `ST`. This is not only pure, but also the safest way to do
-- mutation to the array.
--
-- @since 0.2.2
withMArrayST ::
     Mutable r ix e
  => Array r ix e
  -> (forall s . MArray s r ix e -> ST s a)
  -> Array r ix e
withMArrayST arr f = runST $ withMArrayS arr f
{-# INLINE withMArrayST #-}


-- | /O(1)/ - Lookup an element in the mutable array. Returns `Nothing` when index is out of bounds.
--
-- @since 0.1.0
read :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> ix -> m (Maybe e)
read marr ix =
  if isSafeIndex (msize marr) ix
    then Just <$> unsafeRead marr ix
    else return Nothing
{-# INLINE read #-}


-- | /O(1)/ - Same as `read`, but throws `IndexOutOfBoundsException` on an invalid index.
--
-- @since 0.4.0
readM :: (Mutable r ix e, PrimMonad m, MonadThrow m) =>
        MArray (PrimState m) r ix e -> ix -> m e
readM marr ix =
  read marr ix >>= \case
    Just e -> pure e
    Nothing -> throwM $ IndexOutOfBoundsException (msize marr) ix
{-# INLINE readM #-}


-- | /O(1)/ - Same as `read`, but throws `IndexOutOfBoundsException` on an invalid index.
--
-- @since 0.1.0
read' :: (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> m e
read' marr ix =
  read marr ix >>= \case
    Just e -> pure e
    Nothing -> throw $ IndexOutOfBoundsException (msize marr) ix
{-# INLINE read' #-}
{-# DEPRECATED read' "In favor of more general `readM`" #-}


-- | /O(1)/ - Write an element into the cell of a mutable array. Returns `False` when index is out
-- of bounds.
--
-- @since 0.1.0
write :: (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> e -> m Bool
write marr ix e =
  if isSafeIndex (msize marr) ix
  then unsafeWrite marr ix e >> pure True
  else pure False
{-# INLINE write #-}

-- | /O(1)/ - Same as `write`, but throws `IndexOutOfBoundsException` on an invalid index.
--
-- @since 0.4.0
writeM ::
     (Mutable r ix e, PrimMonad m, MonadThrow m) => MArray (PrimState m) r ix e -> ix -> e -> m ()
writeM marr ix e =
  write marr ix e >>= (`unless` throwM (IndexOutOfBoundsException (msize marr) ix))
{-# INLINE writeM #-}


-- | /O(1)/ - Same as `write`, but lives in IO and throws `IndexOutOfBoundsException` on invalid
-- index.
--
-- @since 0.1.0
write' ::
     (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> e -> m ()
write' marr ix e = write marr ix e >>= (`unless` throw (IndexOutOfBoundsException (msize marr) ix))
{-# INLINE write' #-}
{-# DEPRECATED write' "In favor of more general `writeM`" #-}

-- | /O(1)/ - Modify an element in the cell of a mutable array with a supplied
-- action. Returns the previous value, if index was not out of bounds.
--
-- @since 0.1.0
modify ::
     (Mutable r ix e, PrimMonad m)
  => MArray (PrimState m) r ix e -- ^ Array to mutate.
  -> (e -> m e) -- ^ Monadic action that modifies the element
  -> ix -- ^ Index at which to perform modification.
  -> m (Maybe e)
modify marr f ix =
  if isSafeIndex (msize marr) ix
    then Just <$> unsafeModify marr f ix
    else return Nothing
{-# INLINE modify #-}

-- | /O(1)/ - Modify an element in the cell of a mutable array with a supplied
-- action. Throws an `IndexOutOfBoundsException` exception for invalid index and returns
-- the previous value otherwise.
--
-- @since 0.4.0
modifyM ::
     (Mutable r ix e, PrimMonad m, MonadThrow m)
  => MArray (PrimState m) r ix e -- ^ Array to mutate.
  -> (e -> m e) -- ^ Monadic action that modifies the element
  -> ix -- ^ Index at which to perform modification.
  -> m e
modifyM marr f ix
  | isSafeIndex (msize marr) ix = unsafeModify marr f ix
  | otherwise = throwM (IndexOutOfBoundsException (msize marr) ix)
{-# INLINE modifyM #-}

-- | /O(1)/ - Same as `modifyM`, but discard the returned element
--
-- ====__Examples__
--
-- >>> :set -XTypeApplications
-- >>> import Control.Monad.ST
-- >>> import Data.Massiv.Array
-- >>> runST $ new @P @Ix1 @Int (Sz1 3) >>= (\ma -> modifyM_ ma (pure . (+10)) 1 >> freezeS ma)
-- Array P Seq (Sz1 3)
--   [ 0, 10, 0 ]
--
-- @since 0.4.0
modifyM_ ::
     (Mutable r ix e, PrimMonad m, MonadThrow m)
  => MArray (PrimState m) r ix e -- ^ Array to mutate.
  -> (e -> m e) -- ^ Monadic action that modifies the element
  -> ix -- ^ Index at which to perform modification.
  -> m ()
modifyM_ marr f ix = void $ modifyM marr f ix
{-# INLINE modifyM_ #-}


-- | /O(1)/ - Same as `modify`, but throws an error if index is out of bounds.
--
-- @since 0.1.0
modify' :: (Mutable r ix e, PrimMonad m) =>
        MArray (PrimState m) r ix e -> (e -> e) -> ix -> m ()
modify' marr f ix =
  modify marr (pure . f) ix >>= \case
    Just _ -> pure ()
    Nothing -> throw (IndexOutOfBoundsException (msize marr) ix)
{-# INLINE modify' #-}
{-# DEPRECATED modify' "In favor of more general `modifyM`" #-}


-- | /O(1)/ - Same as `swapM`, but instead of thropwing an exception returns `Nothing` when
-- either one of the indices is out of bounds and `Just` elements under those indices
-- otherwise.
--
-- @since 0.1.0
swap :: (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> ix -> m (Maybe (e, e))
swap marr ix1 ix2 =
  let sz = msize marr
   in if isSafeIndex sz ix1 && isSafeIndex sz ix2
        then Just <$> unsafeSwap marr ix1 ix2
        else pure Nothing
{-# INLINE swap #-}

-- | /O(1)/ - Swap two elements in a mutable array under the supplied indices. Throws an
-- `IndexOutOfBoundsException` when either one of the indices is out of bounds and
-- elements under those indices otherwise.
--
-- @since 0.4.0
swapM ::
     (Mutable r ix e, PrimMonad m, MonadThrow m)
  => MArray (PrimState m) r ix e
  -> ix -- ^ Index for the first element, which will be returned as the first element in the
        -- tuple.
  -> ix -- ^ Index for the second element, which will be returned as the second element in
        -- the tuple.
  -> m (e, e)
swapM marr ix1 ix2
  | not (isSafeIndex sz ix1) = throwM $ IndexOutOfBoundsException (msize marr) ix1
  | not (isSafeIndex sz ix2) = throwM $ IndexOutOfBoundsException (msize marr) ix2
  | otherwise = unsafeSwap marr ix1 ix2
  where
    !sz = msize marr
{-# INLINE swapM #-}


-- | /O(1)/ - Same as `swapM`, but discard the returned elements
--
-- @since 0.4.0
swapM_ ::
     (Mutable r ix e, PrimMonad m, MonadThrow m) => MArray (PrimState m) r ix e -> ix -> ix -> m ()
swapM_ marr ix1 ix2 = void $ swapM marr ix1 ix2
{-# INLINE swapM_ #-}


-- | /O(1)/ - Same as `swap`, but throws an `IndexOutOfBoundsException` on invalid indices.
--
-- @since 0.1.0
swap' ::
     (Mutable r ix e, PrimMonad m) => MArray (PrimState m) r ix e -> ix -> ix -> m ()
swap' marr ix1 ix2 =
  swap marr ix1 ix2 >>= \case
    Just _ -> pure ()
    Nothing ->
      if isSafeIndex (msize marr) ix1
        then throw $ IndexOutOfBoundsException (msize marr) ix2
        else throw $ IndexOutOfBoundsException (msize marr) ix1
{-# INLINE swap' #-}
{-# DEPRECATED swap' "In favor of more general `swapM`" #-}
