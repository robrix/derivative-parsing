module Main where

import Control.Applicative
import Criterion.Main
import Derivative.Parser

main :: IO ()
main = defaultMain
  [ bgroup "many" $ let p = parser (many (char 'a')) in
    [ bench "1" $ whnf (parse p) (replicate 1 'a')
    , bench "10" $ whnf (parse p) (replicate 10 'a')
    , bench "100" $ whnf (parse p) (replicate 100 'a')
    , bench "1000" $ whnf (parse p) (replicate 1000 'a')
    , bench "10000" $ whnf (parse p) (replicate 10000 'a')
    ]
  ]
