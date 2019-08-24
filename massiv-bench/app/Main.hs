{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where


import Data.Massiv.Array as A
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Array.Unsafe as A
import Control.Monad.ST
import qualified Data.Vector.Primitive as VP

main :: IO ()
main = do
  let sz = Sz1 5
      arr = makeArrayR P Seq sz (fromIntegral . subtract 2) :: Array P Ix1 Double
      v = toVector arr
      v' = VP.filter (>0) v
  v' `seq` print (v' VP.! 1)
