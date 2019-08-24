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
{-# LANGUAGE TupleSections #-}
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

import Control.Applicative
import Control.Monad (unless, when, void)
import Control.Monad.ST
import Control.Scheduler as Scheduler
import Control.Scheduler.Internal
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Massiv.Array.Delayed.Pull
import Data.Massiv.Array.Manifest
import Data.Massiv.Array.Manifest.Internal
import Data.Massiv.Array.Ops.Fold.Internal as A
import Data.Massiv.Core.Common
import Data.Massiv.Core.List (L, showArrayList, showsArrayPrec)
import Data.Massiv.Core.Operations
import Data.Primitive.MutVar
import Data.Typeable
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Util
import GHC.Base (build)
import Numeric
import Prelude hiding (zipWith)
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace

data Stream m e = Stream
  { sStream :: S.Stream m e
  , sSize   :: Size
  }

toStream :: forall r ix e m . (Monad m, Source r ix e) => Array r ix e -> Stream m e
toStream arr = k `seq` arr `seq` Stream (S.Stream step 0) (Exact k)
  where
    k = totalElem $ size arr
    step i
      | i < k =
        let e = unsafeLinearIndex arr i
         in e `seq` return $ S.Yield e (i + 1)
      | otherwise = return S.Done
    {-# INLINE step #-}
{-# INLINE toStream #-}

istream :: forall r ix e m . (Monad m, Source r ix e) => Array r ix e -> Stream m (Ix1, e)
istream arr = k `seq` arr `seq` Stream (S.Stream step 0) (Exact k)
  where
    k = totalElem $ size arr
    step i
      | i < k =
        let e = unsafeLinearIndex arr i
         in e `seq` return $ S.Yield (i, e) (i + 1)
      | otherwise = return S.Done
    {-# INLINE step #-}
{-# INLINE istream #-}


fromStreamM :: forall r e m. (Monad m, Mutable r Ix1 e) => Stream m e -> m (Array r Ix1 e)
fromStreamM (Stream str sz) = do
  xs <- S.toList str
  case upperBound sz of
    Nothing -> pure $! unstreamUnknown (S.fromList xs)
    Just k  -> pure $! unstreamMax k (S.fromList xs)
{-# INLINE fromStreamM #-}


traverseA' ::
     (Mutable r ix e, Source r' ix a, Applicative f)
  => (a -> f e)
  -> Array r' ix a
  -> f (Array r ix e)
traverseA' f arr =
  unstreamExact (size arr) . S.fromList <$> Prelude.traverse f xs
  where
    xs = unId (S.toList (sStream (toStream arr)))
{-# INLINE traverseA' #-}


mapM' ::
     (Mutable r ix e, Source r' ix a, Monad m)
  => (a -> m e)
  -> Array r' ix a
  -> m (Array r ix e)
mapM' f arr = fromStreamExactM (size arr) . sStream . mapStrM f . toStream $ arr
{-# INLINE mapM' #-}


fromStream :: forall r e . Mutable r Ix1 e => Stream Id e -> Array r Ix1 e
fromStream (Stream str sz) =
  case upperBound sz of
    Nothing -> unstreamUnknown str
    Just k  -> unstreamMax k str
{-# INLINE fromStream #-}


fromStreamExactM ::
     forall r ix e m. (Monad m, Mutable r ix e)
  => Sz ix
  -> S.Stream m e
  -> m (Array r ix e)
fromStreamExactM sz str = do
  xs <- S.toList str
  pure $! unstreamExact sz (S.fromList xs)
{-# INLINE fromStreamExactM #-}


unstreamExact ::
     forall r ix e. (Mutable r ix e)
  => Sz ix
  -> S.Stream Id e
  -> Array r ix e
unstreamExact sz str =
  runST $ do
    marr <- unsafeNew sz
    _ <- unstreamMaxM marr str
    unsafeFreeze Seq marr
{-# INLINE unstreamExact #-}


unstreamMax ::
     forall r e. (Mutable r Ix1 e)
  => Int
  -> S.Stream Id e
  -> Array r Ix1 e
unstreamMax kMax str = -- @(S.Stream step s) =
  runST $ do
    marr <- unsafeNew (SafeSz kMax)
    k <- unstreamMaxM marr str
    -- let stepLoad t i =
    --       case unId (step t) of
    --         S.Yield e' t' -> do
    --           unsafeLinearWrite marr i e'
    --           stepLoad t' (i + 1)
    --         S.Skip t' -> stepLoad t' i
    --         S.Done -> return i
    --     {-# INLINE stepLoad #-}
    -- k <- stepLoad s 0
    unsafeLinearShrink marr (SafeSz k) >>= unsafeFreeze Seq
{-# INLINE unstreamMax #-}


unstreamUnknown :: Mutable r Ix1 a => S.Stream Id a -> Array r Ix1 a
unstreamUnknown (S.Stream step s) =
  runST $ do
    let kInit = 1
        stepLoad t i kMax marr
          | i < kMax =
            case unId (step t) of
              S.Yield e' t' -> do
                unsafeLinearWrite marr i e'
                stepLoad t' (i + 1) kMax marr
              S.Skip t' -> stepLoad t' i kMax marr
              S.Done -> unsafeLinearShrink marr (SafeSz i)
          | otherwise = do
            let kMax' = kMax * 2
            marr' <- unsafeLinearGrow marr (SafeSz kMax')
            stepLoad t i kMax' marr'
        {-# INLINE stepLoad #-}
    marr <- unsafeNew (SafeSz kInit)
    unsafeFreeze Seq =<< stepLoad s 0 kInit marr
{-# INLINE unstreamUnknown #-}


filterSM ::
     forall r r' ix e m. (Mutable r Ix1 e, Source r' ix e, Monad m)
  => (e -> m Bool)
  -> Array r' ix e
  -> m (Array r Ix1 e)
filterSM f = fromStreamM . filterStrM f . toStream
{-# INLINE filterSM #-}

filterS ::
     forall r r' ix e. (Mutable r Ix1 e, Source r' ix e)
  => (e -> Bool)
  -> Array r' ix e
  -> Array r Ix1 e
filterS f = fromStream . filterStrM (pure . f) . toStream
{-# INLINE filterS #-}

filterStrM :: Monad m => (e -> m Bool) -> Stream m e -> Stream m e
filterStrM f (Stream str k) = Stream (S.filterM f str) (toMax k)
{-# INLINE filterStrM #-}

mapStrM :: Monad m => (e -> m a) -> Stream m e -> Stream m a
mapStrM f (Stream str k) = Stream (S.mapM f str) k
{-# INLINE mapStrM #-}

unstreamMaxM ::
     (Mutable r ix a, PrimMonad m) => MArray (PrimState m) r ix a -> S.Stream Id a -> m Int
unstreamMaxM marr (S.Stream step s) = stepLoad s 0
  where
    stepLoad t i =
      case unId (step t) of
        S.Yield e' t' -> do
          unsafeLinearWrite marr i e'
          stepLoad t' (i + 1)
        S.Skip t' -> stepLoad t' i
        S.Done -> return i
    {-# INLINE stepLoad #-}
{-# INLINE unstreamMaxM #-}

filterStr :: Monad m => (e -> Bool) -> Stream m e -> S.Stream m e
filterStr f (Stream str _) = S.filter f str
{-# INLINE filterStr #-}


filterComp ::
     (Mutable r Ix1 e, Source r' Ix1 e, Source r' ix e)
  => (e -> Bool)
  -> Array r' ix e
  -> Array r Ix1 e
filterComp f arr =
  unsafePerformIO $ do
    let !sz = size arr
        !totalLength = totalElem sz
    marr <- unsafeNew (Sz totalLength)
    let !comp = getComp arr
    chunks <-
      withScheduler comp $ \scheduler ->
        splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
          let scheduleChunkLoad !chunkSz !start =
                scheduleWork scheduler $ do
                  k <-
                    unstreamMaxM
                      (unsafeMutableSlice start chunkSz marr)
                      (filterStr f (toStream (unsafeLinearSlice start chunkSz arr)))
                  pure (start, start + k)
              {-# INLINE scheduleChunkLoad #-}
          loopM_ 0 (< slackStart) (+ chunkLength) (scheduleChunkLoad (SafeSz chunkLength))
          when (slackStart < totalLength) $
            scheduleChunkLoad (SafeSz (totalLength - slackStart)) slackStart
    k <- F.foldlM (moveMisaligned marr marr) 0 chunks
    unsafeLinearShrink marr (SafeSz k) >>= unsafeFreeze comp
{-# INLINE filterComp #-}



-- ifilterComp ::
--      (Mutable r Ix1 e, Source r' Ix1 e, Source r' ix e)
--   => (ix -> e -> Bool)
--   -> Array r' ix e
--   -> Array r Ix1 e
-- ifilterComp f arr =
--   unsafePerformIO $ do
--     let !sz = size arr
--         !totalLength = totalElem sz
--     marr <- unsafeNew (Sz totalLength)
--     let !comp = getComp arr
--     chunks <-
--       withScheduler comp $ \scheduler ->
--         splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--           let scheduleChunkLoad chunkSz start =
--                 scheduleWork scheduler $ do
--                   let arr' = unsafeLinearSlice start chunkSz arr
--                       marr' = unsafeMutableSlice start chunkSz marr
--                       g (i, e) = f (fromLinearIndex sz (i + start)) e
--                       {-# INLINE g #-}
--                   k <- unstreamMaxM marr' (snd <$> filterStr g (istream arr'))
--                   pure (start, start + k)
--               {-# INLINE scheduleChunkLoad #-}
--           loopM_ 0 (< slackStart) (+ chunkLength) (scheduleChunkLoad (SafeSz chunkLength))
--           when (slackStart < totalLength) $
--             scheduleChunkLoad (SafeSz (totalLength - slackStart)) slackStart
--     k <- F.foldlM (moveMisaligned marr marr) 0 chunks
--     unsafeLinearShrink marr (SafeSz k) >>= unsafeFreeze comp
-- {-# INLINE ifilterComp #-}



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

toStreams :: forall r ix e . Source r ix e => Array r ix e -> Array DS Ix1 e
toStreams arr = DSArray comp (Exact k) (Streams (S.Stream chunks 0))
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
{-# INLINE toStreams #-}

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


loadStreamsUpper ::
     forall r ix e m . (Mutable r ix e, PrimMonad m)
  => ((Scheduler m (Ix1, Ix1) -> m ()) -> m [(Ix1, Ix1)])
  -> MArray (PrimState m) r ix e
  -> Streams m e
  -> m (MArray (PrimState m) r Ix1 e)
loadStreamsUpper withScheduler' marr strs = do
  xs <- withScheduler' $ \ scheduler ->
    loadStreams scheduler (unsafeLinearWrite marr) strs
  k <- F.foldlM (moveMisaligned marr marr) 0 xs
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



