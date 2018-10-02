{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
#if __GLASGOW_HASKELL__ >= 800
  {-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
-- Module      : Graphics.ColorSpace.Elevator
-- Copyright   : (c) Alexey Kuleshevich 2018
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.Elevator (
  Elevator(..)
  , clamp01
  ) where

import           Data.Complex         as C
import           Data.Int
import           Data.Typeable
import           Data.Vector.Storable (Storable)
import           Data.Vector.Unboxed  (Unbox)
import           Data.Word
import           GHC.Float


-- | A class with a set of convenient functions that allow for changing precision of
-- channels within pixels, while scaling the values to keep them in an appropriate range.
--
-- >>> let rgb = PixelRGB 0.0 0.5 1.0 :: Pixel RGB Double
-- >>> eToWord8 <$> rgb
-- <RGB:(0|128|255)>
-- >>> eToWord16 <$> rgb
-- <RGB:(0|32768|65535)>
--
class (Eq e, Num e, Num (LevelUp e), Typeable e, Unbox e, Storable e) => Elevator e where
  type LevelUp e :: *

  eUp :: e -> LevelUp e
  default eUp :: Integral e => e -> LevelUp e
  eUp = fromIntegral
  {-# INLINE eUp #-}

  eDown :: LevelUp e -> e
  default eDown :: Integral (LevelUp e) => LevelUp e -> e
  eDown = fromIntegral
  {-# INLINE eDown #-}

  -- | Values are scaled to @[0, 255]@ range.
  eToWord8 :: e -> Word8

  -- | Values are scaled to @[0, 65535]@ range.
  eToWord16 :: e -> Word16

  -- | Values are scaled to @[0, 4294967295]@ range.
  eToWord32 :: e -> Word32

  -- | Values are scaled to @[0, 18446744073709551615]@ range.
  eToWord64 :: e -> Word64

  -- | Values are scaled to @[0.0, 1.0]@ range.
  eToFloat :: e -> Float

  -- | Values are scaled to @[0.0, 1.0]@ range.
  eToDouble :: e -> Double

  -- | Values are scaled from @[0.0, 1.0]@ range.
  eFromDouble :: Double -> e

  -- | Values are scaled to @[0.0, 1.0]@ range.
  eCoerceToDouble :: e -> Double
  default eCoerceToDouble :: Integral e => e -> Double
  eCoerceToDouble = fromIntegral
  {-# INLINE eCoerceToDouble #-}

  -- | Values are scaled from @[0.0, 1.0]@ range.
  eRoundFromDouble :: Double -> e
  default eRoundFromDouble :: Integral e => Double -> e
  eRoundFromDouble = round
  {-# INLINE eRoundFromDouble #-}


-- | Lower the precision
dropDown :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
dropDown !e = fromIntegral $ fromIntegral e `div` ((maxBound :: a) `div`
                                                   fromIntegral (maxBound :: b))
{-# INLINE dropDown #-}

-- | Increase the precision
raiseUp :: forall a b. (Integral a, Bounded a, Integral b, Bounded b) => a -> b
raiseUp !e = fromIntegral e * ((maxBound :: b) `div` fromIntegral (maxBound :: a))
{-# INLINE raiseUp #-}

-- | Convert to fractional with value less than or equal to 1.
squashTo1 :: forall a b. (Fractional b, Integral a, Bounded a) => a -> b
squashTo1 !e = fromIntegral e / fromIntegral (maxBound :: a)
{-# INLINE squashTo1 #-}

-- | Convert to integral streaching it's value up to a maximum value.
stretch :: forall a b. (RealFrac a, Floating a, Integral b, Bounded b) => a -> b
stretch !e = round (fromIntegral (maxBound :: b) * clamp01 e)
{-# INLINE stretch #-}


-- | Clamp a value to @[0, 1]@ range.
clamp01 :: (Ord a, Floating a) => a -> a
clamp01 !x = min (max 0 x) 1
{-# INLINE clamp01 #-}


-- | Values between @[0, 255]]@
instance Elevator Word8 where
  type LevelUp Word8 = Int16
  eToWord8 = id
  {-# INLINE eToWord8 #-}
  eToWord16 = raiseUp
  {-# INLINE eToWord16 #-}
  eToWord32 = raiseUp
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1
  {-# INLINE eToDouble #-}
  eFromDouble = eToWord8
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 65535]]@
instance Elevator Word16 where
  type LevelUp Word16 = Int32
  eToWord8 = dropDown
  {-# INLINE eToWord8 #-}
  eToWord16 = id
  {-# INLINE eToWord16 #-}
  eToWord32 = raiseUp
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1
  {-# INLINE eToDouble #-}
  eFromDouble = eToWord16
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 4294967295]@
instance Elevator Word32 where
  type LevelUp Word32 = Int64
  eToWord8 = dropDown
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown
  {-# INLINE eToWord16 #-}
  eToWord32 = id
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1
  {-# INLINE eToDouble #-}
  eFromDouble = eToWord32
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 18446744073709551615]@
instance Elevator Word64 where
  type LevelUp Word64 = Double
  eDown = round
  {-# INLINE eDown #-}
  eToWord8 = dropDown
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown
  {-# INLINE eToWord16 #-}
  eToWord32 = dropDown
  {-# INLINE eToWord32 #-}
  eToWord64 = id
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1
  {-# INLINE eToDouble #-}
  eFromDouble = eToWord64
  {-# INLINE eFromDouble #-}

-- | Values between @[0, 18446744073709551615]@ on 64bit
instance Elevator Word where
  type LevelUp Word = Double
  eDown = round
  {-# INLINE eDown #-}
  eToWord8 = dropDown
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown
  {-# INLINE eToWord16 #-}
  eToWord32 = dropDown
  {-# INLINE eToWord32 #-}
  eToWord64 = fromIntegral
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}

-- | Values between @[0, 127]@
instance Elevator Int8 where
  type LevelUp Int8 = Int16
  eToWord8 = fromIntegral . max 0
  {-# INLINE eToWord8 #-}
  eToWord16 = raiseUp . max 0
  {-# INLINE eToWord16 #-}
  eToWord32 = raiseUp . max 0
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp . max 0
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1 . max 0
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1 . max 0
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 32767]@
instance Elevator Int16 where
  type LevelUp Int16 = Int32
  eToWord8 = dropDown . max 0
  {-# INLINE eToWord8 #-}
  eToWord16 = fromIntegral . max 0
  {-# INLINE eToWord16 #-}
  eToWord32 = raiseUp . max 0
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp . max 0
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1 . max 0
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1 . max 0
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 2147483647]@
instance Elevator Int32 where
  type LevelUp Int32 = Int64
  eToWord8 = dropDown . max 0
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown . max 0
  {-# INLINE eToWord16 #-}
  eToWord32 = fromIntegral . max 0
  {-# INLINE eToWord32 #-}
  eToWord64 = raiseUp . max 0
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1 . max 0
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1 . max 0
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 9223372036854775807]@
instance Elevator Int64 where
  type LevelUp Int64 = Double
  eDown = round
  {-# INLINE eDown #-}
  eToWord8 = dropDown . max 0
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown . max 0
  {-# INLINE eToWord16 #-}
  eToWord32 = dropDown . max 0
  {-# INLINE eToWord32 #-}
  eToWord64 = fromIntegral . max 0
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1 . max 0
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1 . max 0
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}


-- | Values between @[0, 9223372036854775807]@ on 64bit
instance Elevator Int where
  type LevelUp Int = Double
  eDown = round
  {-# INLINE eDown #-}
  eToWord8 = dropDown . max 0
  {-# INLINE eToWord8 #-}
  eToWord16 = dropDown . max 0
  {-# INLINE eToWord16 #-}
  eToWord32 = dropDown . max 0
  {-# INLINE eToWord32 #-}
  eToWord64 = fromIntegral . max 0
  {-# INLINE eToWord64 #-}
  eToFloat = squashTo1 . max 0
  {-# INLINE eToFloat #-}
  eToDouble = squashTo1 . max 0
  {-# INLINE eToDouble #-}
  eFromDouble = stretch . clamp01
  {-# INLINE eFromDouble #-}


-- | Values between @[0.0, 1.0]@
instance Elevator Float where
  type LevelUp Float = Float
  eUp = id
  {-# INLINE eUp #-}
  eDown = id
  {-# INLINE eDown #-}
  eToWord8 = stretch . clamp01
  {-# INLINE eToWord8 #-}
  eToWord16 = stretch . clamp01
  {-# INLINE eToWord16 #-}
  eToWord32 = stretch . clamp01
  {-# INLINE eToWord32 #-}
  eToWord64 = stretch . clamp01
  {-# INLINE eToWord64 #-}
  eToFloat = id
  {-# INLINE eToFloat #-}
  eToDouble = float2Double
  {-# INLINE eToDouble #-}
  eFromDouble = eToFloat
  {-# INLINE eFromDouble #-}
  eCoerceToDouble = float2Double
  {-# INLINE eCoerceToDouble #-}
  eRoundFromDouble = eToFloat
  {-# INLINE eRoundFromDouble #-}


-- | Values between @[0.0, 1.0]@
instance Elevator Double where
  type LevelUp Double = Double
  eUp = id
  {-# INLINE eUp #-}
  eDown = id
  {-# INLINE eDown #-}
  eToWord8 = stretch . clamp01
  {-# INLINE eToWord8 #-}
  eToWord16 = stretch . clamp01
  {-# INLINE eToWord16 #-}
  eToWord32 = stretch . clamp01
  {-# INLINE eToWord32 #-}
  eToWord64 = stretch . clamp01
  {-# INLINE eToWord64 #-}
  eToFloat = double2Float
  {-# INLINE eToFloat #-}
  eToDouble = id
  {-# INLINE eToDouble #-}
  eFromDouble = id
  {-# INLINE eFromDouble #-}
  eCoerceToDouble = id
  {-# INLINE eCoerceToDouble #-}
  eRoundFromDouble = id
  {-# INLINE eRoundFromDouble #-}


instance Elevator (C.Complex Float) where
  type LevelUp (C.Complex Float) = C.Complex Float
  eUp (r :+ i) = eUp r :+ eUp i
  {-# INLINE eUp #-}
  eDown (r :+ i) = eDown r :+ eDown i
  {-# INLINE eDown #-}
  eToWord8 = eToWord8 . C.realPart
  {-# INLINE eToWord8 #-}
  eToWord16 = eToWord16 . C.realPart
  {-# INLINE eToWord16 #-}
  eToWord32 = eToWord32 . C.realPart
  {-# INLINE eToWord32 #-}
  eToWord64 = eToWord64 . C.realPart
  {-# INLINE eToWord64 #-}
  eToFloat = eToFloat . C.realPart
  {-# INLINE eToFloat #-}
  eToDouble = eToDouble . C.realPart
  {-# INLINE eToDouble #-}
  eFromDouble = (C.:+ 0) . eFromDouble
  {-# INLINE eFromDouble #-}
  eCoerceToDouble = eToDouble
  {-# INLINE eCoerceToDouble #-}
  eRoundFromDouble = eFromDouble
  {-# INLINE eRoundFromDouble #-}

instance Elevator (C.Complex Double) where
  type LevelUp (C.Complex Double) = C.Complex Double
  eUp (r :+ i) = eUp r :+ eUp i
  {-# INLINE eUp #-}
  eDown (r :+ i) = eDown r :+ eDown i
  {-# INLINE eDown #-}
  eToWord8 = eToWord8 . C.realPart
  {-# INLINE eToWord8 #-}
  eToWord16 = eToWord16 . C.realPart
  {-# INLINE eToWord16 #-}
  eToWord32 = eToWord32 . C.realPart
  {-# INLINE eToWord32 #-}
  eToWord64 = eToWord64 . C.realPart
  {-# INLINE eToWord64 #-}
  eToFloat = eToFloat . C.realPart
  {-# INLINE eToFloat #-}
  eToDouble = eToDouble . C.realPart
  {-# INLINE eToDouble #-}
  eFromDouble = (C.:+ 0) . eFromDouble
  {-# INLINE eFromDouble #-}
  eCoerceToDouble = eToDouble
  {-# INLINE eCoerceToDouble #-}
  eRoundFromDouble = eFromDouble
  {-# INLINE eRoundFromDouble #-}


-- -- | Discards imaginary part and changes precision of real part.
-- instance (Num e, Elevator e, RealFloat e) => Elevator (C.Complex e) where
--   type LevelUp (C.Complex e) = C.Complex (LevelUp e)
--   eUp (r :+ i) = eUp r :+ eUp i
--   {-# INLINE eUp #-}
--   eDown (r :+ i) = eDown r :+ eDown i
--   {-# INLINE eDown #-}
--   eToWord8 = eToWord8 . C.realPart
--   {-# INLINE eToWord8 #-}
--   eToWord16 = eToWord16 . C.realPart
--   {-# INLINE eToWord16 #-}
--   eToWord32 = eToWord32 . C.realPart
--   {-# INLINE eToWord32 #-}
--   eToWord64 = eToWord64 . C.realPart
--   {-# INLINE eToWord64 #-}
--   eToFloat = eToFloat . C.realPart
--   {-# INLINE eToFloat #-}
--   eToDouble = eToDouble . C.realPart
--   {-# INLINE eToDouble #-}
--   eFromDouble = (C.:+ 0) . eFromDouble
--   {-# INLINE eFromDouble #-}
