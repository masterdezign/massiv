{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Criterion.Main
import qualified Data.DList as DL
import Data.Massiv.Array as A
import Data.Massiv.Bench as A
import qualified Data.Vector.Primitive as VP
import Prelude as P

main :: IO ()
main = do
  let !sz = Sz (600 :. 1000)
      !arr = computeAs P $ resize' (Sz $ totalElem sz) $ arrRLightIx2 DL Par sz
  defaultMain
    [ mkAppendBenchGroup "LeftToRight" (Dim 1) sz
    , mkAppendBenchGroup "TopToBottom" (Dim 2) sz
    , bgroup
        "Monoid"
        [ bench "mappend" $ whnf (\a -> A.computeAs P (toLoadArray a <> toLoadArray a)) arr
        , bench "appendDL" $ whnfIO (A.computeAs P <$> appendDL (toLoadArray arr) (toLoadArray arr))
        , bench "mconcat" $ whnf (\a -> A.computeAs P (mconcat [toLoadArray a, toLoadArray a])) arr
        ]
    , bgroup
        "cons"
        [ bench ("Array DL Ix1 Int (" ++ show kSmall ++ ")") $
          nf (A.computeAs P . consArray kSmall) empty
        , bench ("VP.Vector Int (" ++ show kSmall ++ ")") $ nf (consVector kSmall) VP.empty
        , bench ("[Int] (" ++ show kSmall ++ ")") $ nf (consList kSmall) []
        ]
    , bgroup
        "snoc"
        [ bench ("Array DL Ix1 Int (" ++ show kSmall ++ ")") $
          nf (A.computeAs P . snocArray kSmall) empty
        , bench ("VP.Vector Int (" ++ show kSmall ++ ")") $ nf (snocVector kSmall) VP.empty
        , bench ("DList Int (" ++ show kSmall ++ ")") $ nf (snocList kSmall) DL.empty
        ]
    , bgroup
        "unfoldr"
        [ bench "Array (DS)" $ whnf (A.computeAs P . A.unfoldr firstK) 0
        , bench "Vector" $ whnf (VP.unfoldr firstK) 0
        ]
    , bgroup
        "unfoldrN"
        [ bench "Array (DL)" $ whnf (A.computeAs P . unfoldrS_ (Sz k) (\i -> (i :: Int, i + 1))) 0
        , bench "Array (DS)" $
          whnf (A.computeAs P . A.unfoldrN (Sz k) (\i -> Just (i :: Int, i + 1))) 0
        , bench "Vector" $ whnf (VP.unfoldrN k (\i -> Just (i :: Int, i + 1))) 0
        ]
    ]
  where
    !k = 1000000 :: Int
    !kSmall = 10000 :: Int
    firstK i =
      if i < k
        then Just (i, i + 1)
        else Nothing
    consList :: Int -> [Int] -> [Int]
    consList 0 !acc = acc
    consList !n !acc = consList (n - 1) (n : acc)
    consArray :: Int -> Array DL Ix1 Int -> Array DL Ix1 Int
    consArray 0 !acc = acc
    consArray !n !acc = consArray (n - 1) (n `A.cons` acc)
    consVector :: Int -> VP.Vector Int -> VP.Vector Int
    consVector 0 !acc = acc
    consVector !n !acc = consVector (n - 1) (n `VP.cons` acc)
    snocList :: Int -> DL.DList Int -> [Int]
    snocList 0 !acc = DL.toList acc
    snocList !n !acc = snocList (n - 1) (acc `DL.snoc` n)
    snocArray :: Int -> Array DL Ix1 Int -> Array DL Ix1 Int
    snocArray 0 !acc = acc
    snocArray !n !acc = snocArray (n - 1) (acc `A.snoc` n)
    snocVector :: Int -> VP.Vector Int -> VP.Vector Int
    snocVector 0 !acc = acc
    snocVector !n !acc = snocVector (n - 1) (acc `VP.snoc` n)

mkAppendBenchGroup :: String -> Dim -> Sz2 -> Benchmark
mkAppendBenchGroup gname dim sz =
  bgroup
    ("Append " ++ gname)
    [ env (return (arrRLightIx2 P Seq sz)) $ \arr ->
        bgroup
          "Seq"
          [ bench "append" $ whnf (A.computeAs P . append' dim arr) arr
          , bench "concat" $ whnf (A.computeAs P . A.concat' dim . (: [arr])) arr
          -- , bench "appendLoad" $ whnf (A.computeAs P . fromJust . appendLoad dim arr) arr
          -- , bench "appendPull" $ whnf (A.computeAs P . fromJust . appendPull dim arr) arr
          ]
    , env (return (arrRLightIx2 P Par sz)) $ \arr ->
        bgroup
          "Par"
          [ bench "append" $ whnf (A.computeAs P . append' dim arr) arr
          , bench "concat" $ whnf (A.computeAs P . A.concat' dim . (: [arr])) arr
          -- , bench "appendLoad" $ whnf (A.computeAs P . fromJust . appendLoad dim arr) arr
          -- , bench "appendPull" $ whnf (A.computeAs P . fromJust . appendPull dim arr) arr
          ]
    ]
