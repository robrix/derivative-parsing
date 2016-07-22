module Main where

import Control.Applicative
import Criterion.Main
import Derivative.Parser

main :: IO ()
main = defaultMain
  [ bgroup "many"
    [ bench "1" $ whnf (parse (parser (many (char 'a')))) (replicate 1 'a')
    , bench "10" $ whnf (parse (parser (many (char 'a')))) (replicate 10 'a')
    , bench "100" $ whnf (parse (parser (many (char 'a')))) (replicate 100 'a')
    , bench "1000" $ whnf (parse (parser (many (char 'a')))) (replicate 1000 'a')
    ]
  ]
