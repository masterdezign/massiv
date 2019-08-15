{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.Delayed.Stream as A
import Data.Massiv.Array.Manifest.Vector as A
import Data.Massiv.Bench as A
import qualified Data.Vector.Primitive as VP
import Prelude as P

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
      !arr = computeAs P $ resize' (Sz $ totalElem sz) $ arrRLightIx2 DL Seq sz
      !v = A.toVector arr :: VP.Vector Double
  defaultMain
    [ bgroup "Vector" [bench "filter >=0" $ whnf (VP.filter (> 0)) v]
    , bgroup
        "Array"
        [ bench "filter with foldlS" $ whnf (computeAs P . filterA (> 0)) arr
        , bench "filter with foldrS" $ whnf (computeAs P . filterA' (> 0)) arr
        , bench "DS (Seq)" $ whnf (computeS @P . A.filter (> 0)) (toStream arr)
        , bench "DS (Par)" $ whnf (computeAs P . A.filter (> 0)) (toStream (setComp Par arr))
        ]
    ]

-- Really slow
filterA :: Source r ix e => (e -> Bool) -> Array r ix e -> Array DL Ix1 e
filterA f = foldlS collect A.empty
  where
    collect !acc e
      | f e = A.snoc acc e
      | otherwise = acc

-- Slow
filterA' :: Source r ix e => (e -> Bool) -> Array r ix e -> Array DL Ix1 e
filterA' f = foldrS collect A.empty
  where
    collect e !acc
      | f e = A.cons e acc
      | otherwise = acc
