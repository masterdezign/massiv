{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Push
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Push
  ( DL(..)
  , Array(..)
  , toLoadArray
  , makeLoadArrayS
  , makeLoadArray
  , unsafeMakeLoadArray
  , fromStrideLoad
  ) where

import Control.Monad
import Data.Massiv.Core.Common
import Data.Massiv.Core.Index.Internal (Sz(SafeSz))
import Prelude hiding (map, zipWith)
import Control.Scheduler as S (traverse_)
import Data.Foldable as F

#include "massiv.h"

-- | Delayed load representation. Also known as Push array.
data DL = DL deriving Show


data instance Array DL ix e = DLArray
  { dlComp    :: !Comp
  , dlSize    :: !(Sz ix)
  , dlDefault :: !(Maybe e)
  , dlLoad    :: forall m . Monad m
              => Scheduler m ()
              -> Int -- start loading at this linear index
              -> (Int -> e -> m ()) -- linear element writing action
              -> m ()
  }

instance Index ix => Construct DL ix e where
  makeArrayLinear comp sz f =
    DLArray comp sz Nothing $ \scheduler startAt dlWrite ->
      splitLinearlyWithStartAtM_ scheduler startAt (totalElem sz) (pure . f) dlWrite
  {-# INLINE makeArrayLinear #-}

instance Index ix => Resize DL ix where
  unsafeResize !sz arr = arr { dlSize = sz }
  {-# INLINE unsafeResize #-}

instance Semigroup (Array DL Ix1 e) where
  (<>) = mappendDL
  {-# INLINE (<>) #-}

instance Monoid (Array DL Ix1 e) where
  mempty =
    DLArray
      {dlComp = mempty, dlSize = Sz zeroIndex, dlDefault = Nothing, dlLoad = \_ _ _ -> pure ()}
  {-# INLINE mempty #-}
  mappend = mappendDL
  {-# INLINE mappend #-}
  mconcat [] = mempty
  mconcat [x] = x
  mconcat [x, y] = x <> y
  mconcat xs = mconcatDL xs
  {-# INLINE mconcat #-}

mconcatDL :: forall e . [Array DL Ix1 e] -> Array DL Ix1 e
mconcatDL !arrs =
  DLArray {dlComp = foldMap getComp arrs, dlSize = SafeSz k, dlDefault = Nothing, dlLoad = load}
  where
    !k = F.foldl' (+) 0 (unSz . size <$> arrs)
    load :: Monad m => Scheduler m () -> Int -> (Int -> e -> m ()) -> m ()
    load scheduler startAt dlWrite =
      let loadArr !startAtCur DLArray {dlSize = SafeSz kCur, dlDefault, dlLoad} = do
            let !endAtCur = startAtCur + kCur
            scheduleWork_ scheduler $ do
              S.traverse_
                (\def -> loopM_ startAtCur (< endAtCur) (+ 1) (`dlWrite` def))
                dlDefault
              dlLoad scheduler startAtCur dlWrite
            pure endAtCur
          {-# INLINE loadArr #-}
       in foldM_ loadArr startAt arrs
    {-# INLINE load #-}
{-# INLINE mconcatDL #-}


mappendDL :: forall e . Array DL Ix1 e -> Array DL Ix1 e -> Array DL Ix1 e
mappendDL (DLArray c1 sz1 mDef1 load1) (DLArray c2 sz2 mDef2 load2) =
  DLArray {dlComp = c1 <> c2, dlSize = SafeSz (k1 + k2), dlDefault = Nothing, dlLoad = load}
  where
    !k1 = unSz sz1
    !k2 = unSz sz2
    load :: Monad m => Scheduler m () -> Int -> (Int -> e -> m ()) -> m ()
    load scheduler startAt dlWrite = do
      scheduleWork_ scheduler $ do
        S.traverse_ (\def1 -> loopM_ startAt (< k1) (+ 1) (`dlWrite` def1)) mDef1
        load1 scheduler startAt dlWrite
      scheduleWork_ scheduler $ do
        let startAt2 = startAt + k1
        S.traverse_ (\def2 -> loopM_ startAt2 (< startAt2 + k2) (+ 1) (`dlWrite` def2)) mDef2
        load2 scheduler startAt2 dlWrite
    {-# INLINE load #-}
{-# INLINE mappendDL #-}

-- | Describe how an array should be loaded into memory sequentially. For parallelizable
-- version see `makeLoadArray`.
--
-- @since 0.3.1
makeLoadArrayS ::
     Index ix =>
     Sz ix
  -- ^ Size of the resulting array
  -> e
  -- ^ Default value to use for all cells that might have been ommitted by the writing function
  -> (forall m. Monad m => (ix -> e -> m Bool) -> m ())
  -- ^ Writing function that described which elements to write into the target array.
  -> Array DL ix e
makeLoadArrayS sz defVal writer =
  DLArray Seq sz (Just defVal) $ \_scheduler !startAt uWrite ->
    let safeWrite !ix !e
          | isSafeIndex sz ix = uWrite (startAt + toLinearIndex sz ix) e >> pure True
          | otherwise = pure False
        {-# INLINE safeWrite #-}
     in writer safeWrite
{-# INLINE makeLoadArrayS #-}

-- | Specify how an array should be loaded into memory. Unlike `makeLoadArrayS`, loading
-- function accepts a scheduler, thus can be parallelized. If you need an unsafe version
-- of this function see `unsafeMakeLoadArray`.
--
-- @since 0.4.0
makeLoadArray ::
     Index ix
  => Comp
  -- ^ Computation strategy to use. Directly affects the scheduler that gets created for
  -- the loading function.
  -> Sz ix
  -- ^ Size of the resulting array
  -> e
  -- ^ Default value to use for all cells that might have been ommitted by the writing function
  -> (forall m. Monad m =>
                  Scheduler m () -> (ix -> e -> m Bool) -> m ())
  -- ^ Writing function that described which elements to write into the target array. It
  -- accepts a scheduler, that can be used for parallelization, as well as a safe element
  -- writing function.
  -> Array DL ix e
makeLoadArray comp sz defVal writer =
  DLArray comp sz (Just defVal) $ \scheduler !startAt uWrite ->
    let safeWrite !ix !e
          | isSafeIndex sz ix = uWrite (startAt + toLinearIndex sz ix) e >> pure True
          | otherwise = pure False
        {-# INLINE safeWrite #-}
     in writer scheduler safeWrite
{-# INLINE makeLoadArray #-}

-- | Specify how an array can be loaded/computed through creation of a `DL` array. Unlike
-- `makeLoadArrayS` or `makeLoadArray` this function is unsafe, since there is no
-- guarantee that all elements will be initialized and the supplied element writing
-- function does not perform any bounds checking.
--
-- @since 0.3.1
unsafeMakeLoadArray ::
     Comp
  -- ^ Computation strategy to use. Directly affects the scheduler that gets created for
  -- the loading function.
  -> Sz ix
  -- ^ Size of the array
  -> Maybe e
  -- ^ An element to use for initialization of the mutable array that will be created in
  -- the future
  -> (forall m. Monad m =>
                  Scheduler m () -> Int -> (Int -> e -> m ()) -> m ())
  -- ^ This function accepts:
  --
  -- * A scheduler that can be used for parallelization of loading
  --
  -- * Linear index at which this load array will start (an offset that should be added to
  --   the linear writng function)
  --
  -- * Linear element writing function
  -> Array DL ix e
unsafeMakeLoadArray = DLArray
{-# INLINE unsafeMakeLoadArray #-}

-- | Convert any `Load`able array into `DL` representation.
--
-- @since 0.3.0
toLoadArray :: Load r ix e => Array r ix e -> Array DL ix e
toLoadArray arr =
  DLArray (getComp arr) (size arr) Nothing $ \scheduler startAt dlWrite ->
    loadArrayM scheduler arr (\ !i -> dlWrite (i + startAt))
{-# INLINE toLoadArray #-}

-- | Convert an array that can be loaded with stride into `DL` representation.
--
-- @since 0.3.0
fromStrideLoad
  :: StrideLoad r ix e => Stride ix -> Array r ix e -> Array DL ix e
fromStrideLoad stride arr =
  DLArray (getComp arr) newsz Nothing $ \scheduler startAt dlWrite ->
    loadArrayWithStrideM scheduler stride newsz arr (\ !i -> dlWrite (i + startAt))
  where
    newsz = strideSize stride (size arr)
{-# INLINE fromStrideLoad #-}

instance Index ix => Load DL ix e where
  size = dlSize
  {-# INLINE size #-}
  getComp = dlComp
  {-# INLINE getComp #-}
  setComp c arr = arr {dlComp = c}
  {-# INLINE setComp #-}
  loadArrayM scheduler DLArray {dlLoad} = dlLoad scheduler 0
  {-# INLINE loadArrayM #-}
  defaultElement = dlDefault
  {-# INLINE defaultElement #-}

instance Functor (Array DL ix) where
  fmap f arr =
    arr
      { dlLoad =
          \scheduler startAt uWrite -> dlLoad arr scheduler startAt (\ !i e -> uWrite i (f e))
      , dlDefault = f <$> dlDefault arr
      }
  {-# INLINE fmap #-}
