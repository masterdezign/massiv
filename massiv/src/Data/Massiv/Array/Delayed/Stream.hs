{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.Massiv.Array.Delayed.Stream
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Delayed.Stream where
  -- ( Stream(..)
  -- , Array(..)
  -- ) where

import System.IO.Unsafe (unsafePerformIO)
import Data.Primitive.MutVar
import Control.Scheduler.Internal
import Control.Monad (unless, void)
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.Massiv.Array.Ops.Fold.Internal as A
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Manifest
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Core.Common
import Data.Massiv.Core.List (L, showArrayList, showsArrayPrec)
import Data.Massiv.Core.Operations
import Data.Typeable
import qualified Data.Vector.Fusion.Stream.Monadic as S
import GHC.Base (build)
import Numeric
import Prelude hiding (zipWith)
import Control.Scheduler as Scheduler
import Data.List.NonEmpty

import Debug.Trace

--data Streams m e = Streams !(S.Stream m e) ![Ix1, S.Stream m e)]

--data Streams m e = forall s. Streams (s -> m (S.Step s e)) [(Ix1, s)]

newtype Streams m e = Streams (S.Stream m (Ix1, S.Stream m e))

data DS = DS

data instance Array DS Ix1 e = DSArray
  { dsComp :: !Comp
  , dsSize :: !Size
  , dsStreams :: !(forall m . Monad m => Streams m e)
  }

-- data Streams m e where
--   Stream :: (Ix1 -> S.Stream m e) -> Ix1 -> Streams m e
--   None  :: Streams m e

instance (Ragged L Ix1 e, Show e) => Show (Array DS Ix1 e) where
  showsPrec = showsArrayPrec (computeAs B)
  showList = showArrayList


instance Construct DS Ix1 e where
  -- makeArray comp sz f = toStream (makeArrayR D comp sz f)
  -- makeArray comp (Sz k) f = DSArray comp (Exact k) (Streams (S.generate k f) [])
  -- {-# INLINE makeArray #-}


mapMaybe :: (a -> Maybe e) -> Array DS Ix1 a -> Array DS Ix1 e
mapMaybe f arr =
  arr {dsSize = toMax (dsSize arr), dsStreams = liftStreams (S.mapMaybe f) (dsStreams arr)}
{-# INLINE mapMaybe #-}


liftStreams :: Monad m => (S.Stream m a -> S.Stream m b) -> Streams m a -> Streams m b
liftStreams f (Streams ss) = Streams (fmap (fmap f) ss)
{-# INLINE liftStreams #-}

filter :: (e -> Bool) -> Array DS Ix1 e -> Array DS Ix1 e
filter f = mapMaybe (\e -> if f e then Just e else Nothing)
{-# INLINE filter #-}

-- | Generate a stream from its indices
unsafeGenerateFromToM :: Monad m => Int -> Int -> (Int -> m a) -> S.Stream m a
unsafeGenerateFromToM !offset !n f = S.Stream step offset
  where
    step i
      | i < n = do
        x <- f i
        pure $ S.Yield x (i + 1)
      | otherwise = pure S.Done
    {-# INLINE step #-}
{-# INLINE unsafeGenerateFromToM #-}

unsafeGenerateFromTo :: Monad m => Int -> Int -> (Int -> a) -> S.Stream m a
unsafeGenerateFromTo offset n f = unsafeGenerateFromToM offset n (pure . f)
{-# INLINE unsafeGenerateFromTo #-}

toStream :: forall r ix e . Source r ix e => Array r ix e -> Array DS Ix1 e
toStream arr = DSArray comp (Exact k) (Streams (S.Stream chunks 0))
  where
    !k = totalElem $ size arr
    !comp = getComp arr
    !n = unsafePerformIO $ getCompWorkers comp
    !(q, r) = quotRem k n
    !lastChunkStart = k - r - q
    chunks :: Monad m => Int -> m (S.Step Int (Int, S.Stream m e))
    chunks i
      | i <= lastChunkStart =
        let !q' = i + q
        in pure $ S.Yield (i, unsafeGenerateFromTo i q' (unsafeLinearIndex arr)) q'
      | i < k = pure $ S.Yield (i, unsafeGenerateFromTo i k (unsafeLinearIndex arr)) (i + r)
      | otherwise = pure S.Done
    {-# INLINE chunks #-}
{-# INLINE toStream #-}

linearIndex :: Source r ix e => Array r ix e -> Int -> e
linearIndex arr i
  | i < totalElem (size arr) = unsafeLinearIndex arr i
  | otherwise = error $ "Out of bounds: " ++ show i ++ "   " ++ show (totalElem (size arr))

loadStreams_ :: Monad m => Scheduler m () -> (Ix1 -> e -> m ()) -> Streams m e -> m ()
loadStreams_ scheduler write (Streams strs) =
  S.mapM_ (\(i, s) -> scheduleWork_ scheduler $ void $ loadStream write i s) strs
{-# INLINE loadStreams_ #-}

loadStreams'_ :: Monad m => Scheduler m () -> (Ix1 -> e -> m ()) -> Streams m e -> m ()
loadStreams'_ _scheduler write (Streams strs) =
  void $ loadStream write 0 $ S.concatMap snd strs
  --S.mapM_ (uncurry (loadStream write)) strs
{-# INLINE loadStreams'_ #-}

-- | Load the stream using a writing function starting at an index. Returns the index where it
-- has stopped.
loadStream :: Monad m => (Ix1 -> e -> m ()) -> Ix1 -> S.Stream m e -> m Ix1
loadStream write = S.foldlM' (\i e -> (i + 1) <$ write i e)
{-# INLINE loadStream #-}

loadStreams :: Monad m => Scheduler m (Ix1, Ix1) -> (Ix1 -> e -> m ()) -> Streams m e -> m ()
loadStreams scheduler write (Streams strs) =
  S.mapM_ (\(i, s) -> scheduleWork scheduler ((i, ) <$> loadStream write i s)) strs
{-# INLINE loadStreams #-}

-- loadStreamsWithScheduler ::
--      (Mutable r ix e, PrimMonad m)
--   => ((Scheduler m (Ix1, Ix1) -> m ()) -> m [(Ix1, Ix1)])
--   -> MArray (PrimState m) r ix e
--   -> Streams m e
--   -> m (NonEmpty Ix1)
-- loadStreamsWithScheduler withScheduler' marr strs = do
--   withScheduler' $ \scheduler -> do
--       loadStreams' scheduler (unsafeLinearWrite marr) strs
--       S.foldlM
--         (\_ (i, s) -> scheduleWork scheduler $ loadStream (unsafeLinearWrite marr) i s)
--         []
--         strs
--   case ys of
--     [] -> pure (0 :| [])
--     (x:xs) -> pure (x :| xs)
-- {-# INLINE loadStreams #-}

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

moveMisaligned ::
     (PrimMonad m, Mutable r ix e)
  => MArray (PrimState m) r ix e
  -> Ix1
  -> (Ix1, Ix1)
  -> m Ix1
moveMisaligned marr prevEnd (start, end) = do
  unless (prevEnd == start) $ unsafeLinearMove marr start marr prevEnd (Sz (end - start))
  pure (prevEnd + end - start)
{-# INLINE moveMisaligned #-}

loadStreamsUpper ::
     forall r ix e m . (Mutable r ix e, PrimMonad m)
  => ((Scheduler m (Ix1, Ix1) -> m ()) -> m [(Ix1, Ix1)])
  -> MArray (PrimState m) r ix e
  -> Streams m e
  -> m (MArray (PrimState m) r Ix1 e)
loadStreamsUpper withScheduler' marr strs = do
  xs <- withScheduler' $ \ scheduler ->
    loadStreams scheduler (unsafeLinearWrite marr) strs
  k <- F.foldlM (moveMisaligned marr) 0 xs
  pure $ unsafeMutableSlice 0 (SafeSz k) marr
  -- case ys of
  --   [] -> unsafeMutableResize zeroSz <$> unsafeNew (zeroSz :: Sz ix)
  --   (_, firstEnd):xs -> do
{-# INLINE loadStreamsUpper #-}

loadWhileGrowing ::
     (Mutable r Ix1 e, PrimMonad m)
  => (MArray (PrimState m) r Ix1 e, Ix1)
  -> e
  -> m (MArray (PrimState m) r Ix1 e, Ix1)
loadWhileGrowing (marr, i) e = do
  marr' <-
    if i < k
      then pure marr
      else unsafeLinearGrow marr (SafeSz (k + max 1 k))
  unsafeLinearWrite marr' i e
  pure (marr', i + 1)
  where
    k = unSz (msize marr)
{-# INLINE loadWhileGrowing #-}

-- loadStreamsUnknown ::
--      (Mutable r Ix1 e, PrimMonad m)
--   => MArray (PrimState m) r Ix1 e
--   -> Streams m e
--   -> m (MArray (PrimState m) r Ix1 e)
-- loadStreamsUnknown marr (Streams s ss) = do
--   (marr', k') <- F.foldlM (S.foldlM loadWhileGrowing) (marr, 0) (s : Prelude.map snd ss)
--   pure (unsafeMutableSlice 0 (SafeSz k') marr')
-- {-# INLINE loadStreamsUnknown #-}


instance Load DS Ix1 e where
  getComp = dsComp
  {-# INLINE getComp #-}

  setComp comp arr = arr { dsComp = comp }
  {-# INLINE setComp #-}

  -- size (DSArray _ sz (Streams s ss)) =
  --   case sz of
  --     Exact k -> SafeSz k
  --     _ -> SafeSz (runIdentity (S.length s) +
  --                  F.foldl' (+) 0 (runIdentity (mapM (S.length . snd) ss)))
  -- {-# INLINE size #-}

  maxSize arr = SafeSz <$> upperBound (dsSize arr)
  {-# INLINE maxSize #-}

  loadArrayM !scheduler (DSArray _ sz strs) write =
    case sz of
      Exact _ -> loadStreams'_ scheduler write strs
      _       -> error $ "Loading of streams with unknown size is not supported by loadArrayM." ++
                         "Use `unsafeLoadInto` instead"
  {-# INLINE loadArrayM #-}

  unsafeLoadIntoS marr (DSArray _ sz strs) =
    case sz of
      Exact _ -> marr <$ loadStreams'_ trivialScheduler_ (unsafeLinearWrite marr) strs
      Max _ -> loadStreamsUpper withTrivialScheduler marr strs
      --Unknown -> loadStreamsUnknown marr strs
  {-# INLINE unsafeLoadIntoS #-}

  unsafeLoadInto marr (DSArray comp sz strs) =
    case sz of
      Exact _ -> marr <$ liftIO (withScheduler_ comp $ \scheduler ->
                                    loadStreams_ scheduler (unsafeLinearWrite marr) strs)
      Max _ -> liftIO (loadStreamsUpper (withScheduler comp) marr strs)
      --Unknown -> liftIO $ loadStreamsUnknown marr strs
  {-# INLINE unsafeLoadInto #-}


withTrivialScheduler :: PrimMonad m => (Scheduler m a -> m b) -> m [a]
withTrivialScheduler action = do
  resVar <- newMutVar []
  _ <- action $ Scheduler
    { _numWorkers = 1
    , _scheduleWorkId = \f -> do
        res <- f (WorkerId 0)
        modifyMutVar' resVar (res:)
    , _terminate = \e -> writeMutVar resVar [e] >> pure e
    , _terminateWith = \e -> modifyMutVar' resVar (e:) >> pure e
    }
  Prelude.reverse <$> readMutVar resVar
{-# INLINE withTrivialScheduler #-}





-- zipWithM ::
--      Monad m
--   => (e1 -> e2 -> m e3)
--   -> Array (Str m) Ix1 e1
--   -> Array (Str m) Ix1 e2
--   -> Array (Str m) Ix1 e3
-- zipWithM f (StreamV k1 str1) (StreamV k2 str2) =
--   StreamV (liftLength2 min k1 k2) (S.zipWithM f str1 str2)

-- dropWhileM :: Monad m => (e -> m Bool) -> Array (Str m) Ix1 e -> Array (Str m) Ix1 e
-- dropWhileM f (StreamV k str) = StreamV k (S.dropWhileM f str)



-- class Stream r ix e where

--   toStream :: Index ix => Array r ix e -> Array (Str m) Ix1 e

-- instance Stream (Str Identity) Ix1 e where

--   toStream :: Array (Str m) Ix1 e -> Array (Str m) Ix1 e
--   toStream = id
