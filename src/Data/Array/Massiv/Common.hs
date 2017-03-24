{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Module      : Data.Array.Massiv.Common
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Common
  ( Array
  , Massiv(..)
  , Source(..)
  , loop
  , loopM_
  , module Data.Array.Massiv.Index
  ) where

import Data.Array.Massiv.Index

data family Array r ix e :: *


-- | Immutable, shape polymorphic array construction and indexing.
class Index ix => Massiv r ix where

  size :: Array r ix e -> ix



instance Massiv r ix => Show (Array r ix e) where
  show arr = "<Array: " ++ show (size arr) ++ ">"


class Massiv r ix => Source r ix where
  type Elt r ix e :: *
  type Elt r ix e = Array r (Lower ix) e

  unsafeIndex :: Massiv r ix => Array r ix e -> ix -> e
  unsafeIndex !arr = unsafeLinearIndex arr . toLinearIndex (size arr)
  {-# INLINE unsafeIndex #-}

  unsafeLinearIndex :: Array r ix e -> Int -> e
  unsafeLinearIndex !arr = unsafeIndex arr . fromLinearIndex (size arr)
  {-# INLINE unsafeLinearIndex #-}

  (!?) :: Array r ix e -> Int -> Maybe (Elt r ix e)




-- | Very efficient loop
loop :: t -> (t -> Bool) -> (t -> t) -> a -> (t -> a -> a) -> a
loop !init' condition increment !initAcc f = go init' initAcc where
  go !step !acc =
    case condition step of
      False -> acc
      True  -> go (increment step) (f step acc)
{-# INLINE loop #-}


-- | Very efficient monadic loop
loopM_ :: Monad m => t -> (t -> Bool) -> (t -> t) -> (t -> m a) -> m ()
loopM_ !init' condition increment f = go init' where
  go !step =
    case condition step of
      False -> return ()
      True  -> f step >> go (increment step)
{-# INLINE loopM_ #-}

