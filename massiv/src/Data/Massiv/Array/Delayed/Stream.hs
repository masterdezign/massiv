{-# LANGUAGE InstanceSigs #-}
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

data Streams m e = Streams !(S.Stream m e) ![(Ix1, S.Stream m e)]

data DS = DS

data instance Array DS Ix1 e = DSArray
  { dsComp :: !Comp
  , dsSize :: !Size
  , dsStreams :: !(forall m . Monad m => Streams m e)
  }


instance (Ragged L Ix1 e, Show e) => Show (Array DS Ix1 e) where
  showsPrec = showsArrayPrec (computeAs B)
  showList = showArrayList


instance Construct DS Ix1 e where
  makeArray comp (Sz k) f = DSArray comp (Exact k) (Streams (S.generate k f) [])
  {-# INLINE makeArray #-}


mapMaybe :: (a -> Maybe e) -> Array DS Ix1 a -> Array DS Ix1 e
mapMaybe f arr =
  arr {dsSize = toMax (dsSize arr), dsStreams = liftStreams (S.mapMaybe f) (dsStreams arr)}

liftStreams :: (S.Stream m a -> S.Stream m b) -> Streams m a -> Streams m b
liftStreams f (Streams s ss) = Streams (f s) (fmap f <$> ss)

filterA :: (e -> Bool) -> Array DS Ix1 e -> Array DS Ix1 e
filterA f = mapMaybe (\e -> if f e then Just e else Nothing)

toStream :: forall r ix e . Source r ix e => Array r ix e -> Array DS Ix1 e
toStream arr = DSArray comp (Exact k) strs
  where
    k = totalElem $ size arr
    comp = getComp arr
    n = unsafePerformIO $ getCompWorkers comp
    strs :: Monad m => Streams m e
    strs =
      case quotRem k n of
        (0, _) -> Streams (S.generate k (unsafeLinearIndex arr)) []
        (1, 0) -> Streams (S.generate k (unsafeLinearIndex arr)) []
        (q, r) ->
          let go !i !offset !acc
                | 1 < i =
                  go
                    (i - 1)
                    (offset - q)
                    ((offset, S.generate q (\ix -> unsafeLinearIndex arr (ix + offset))) : acc)
                | otherwise = acc
              slackStart = k - r
              slack =
                if r == 0
                  then []
                  else [(slackStart, S.generate r (\ix -> unsafeLinearIndex arr (ix + slackStart)))]
           in Streams (S.generate q (unsafeLinearIndex arr)) (go n (slackStart - q) slack)



-- | Load the stream into array starting with an index. It is expected that the stream
-- does not produce more elements than array is capable of handling.
loadStream :: Monad m => (Ix1 -> e -> m ()) -> Ix1 -> S.Stream m e -> m Ix1
loadStream write = S.foldlM (\i e -> write i e >> pure (i + 1))

-- | Should only be used when the size of mutable array matches the sie of the stream
-- exactly
loadStreams_ :: Monad m => Scheduler m () -> (Ix1 -> e -> m ()) -> Streams m e -> m ()
loadStreams_ scheduler write (Streams str strs) = do
  scheduleWork_ scheduler (void $ loadStream write 0 str)
  Scheduler.traverse_ (scheduleWork_ scheduler . void . uncurry (loadStream write)) strs

loadStreams ::
     (Mutable r ix e, PrimMonad m)
  => ((Scheduler m Ix1 -> m ()) -> m [Ix1])
  -> MArray (PrimState m) r ix e
  -> Streams m e
  -> m (NonEmpty Ix1)
loadStreams withScheduler' marr (Streams str strs) = do
  ys <-
    withScheduler' $ \scheduler -> do
      scheduleWork scheduler (loadStream (unsafeLinearWrite marr) 0 str)
      Scheduler.traverse_
        (scheduleWork scheduler . uncurry (loadStream (unsafeLinearWrite marr)))
        strs
  case ys of
    [] -> error "Impossible <loadStreams>: returned sizes from Scheduler are empty"
    (x:xs) -> pure (x :| xs)

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

moveMisaligned ::
     (PrimMonad m, Mutable r ix e)
  => MArray (PrimState m) r ix e
  -> Ix1
  -> (Ix1, Ix1)
  -> m Ix1
moveMisaligned marr prevEnd (start, end) = do
  unless (prevEnd == start) $ unsafeLinearMove marr start marr prevEnd (Sz (end - start))
  pure (prevEnd + end - start)

loadStreamsUpper ::
     (Mutable r ix e, PrimMonad m)
  => ((Scheduler m Ix1 -> m ()) -> m [Ix1])
  -> MArray (PrimState m) r ix e
  -> Streams m e
  -> m (MArray (PrimState m) r Ix1 e)
loadStreamsUpper withScheduler' marr strs@(Streams _ ss) = do
  firstEnd :| ends <- loadStreams withScheduler' marr strs
  k <- F.foldlM (moveMisaligned marr) firstEnd (Prelude.zip (Prelude.map fst ss) ends)
  pure $ unsafeMutableSlice 0 (SafeSz k) marr


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

loadStreamsUnknown ::
     (Mutable r Ix1 e, PrimMonad m)
  => MArray (PrimState m) r Ix1 e
  -> Streams m e
  -> m (MArray (PrimState m) r Ix1 e)
loadStreamsUnknown marr (Streams s ss) = do
  (marr', k') <- F.foldlM (S.foldlM loadWhileGrowing) (marr, 0) (s : Prelude.map snd ss)
  pure (unsafeMutableSlice 0 (SafeSz k') marr')


instance Load DS Ix1 e where
  getComp = dsComp

  setComp comp arr = arr { dsComp = comp }

  size (DSArray _ sz (Streams s ss)) =
    case sz of
      Exact k -> SafeSz k
      _ -> SafeSz (runIdentity (S.length s) +
                   F.foldl' (+) 0 (runIdentity (mapM (S.length . snd) ss)))

  maxSize arr = SafeSz <$> upperBound (dsSize arr)

  loadArrayM !scheduler (DSArray _ sz strs) write =
    case sz of
      Exact _ -> loadStreams_ scheduler write strs
      _       -> error $ "Loading of streams with unknown size is not supported by loadArrayM." ++
                         "Use `unsafeLoadInto` instead"
  {-# INLINE loadArrayM #-}

  unsafeLoadIntoS marr (DSArray _ sz strs) =
    case sz of
      Exact _ -> marr <$ loadStreams_ trivialScheduler_ (unsafeLinearWrite marr) strs
      Max _ -> loadStreamsUpper withTrivialScheduler marr strs
      Unknown -> loadStreamsUnknown marr strs

  unsafeLoadInto marr (DSArray comp sz strs) =
    case sz of
      Exact _ -> marr <$ liftIO (withScheduler_ comp $ \scheduler ->
                                    loadStreams_ scheduler (unsafeLinearWrite marr) strs)
      Max _ -> liftIO (loadStreamsUpper (withScheduler comp) marr strs)
      Unknown -> liftIO $ loadStreamsUnknown marr strs


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
