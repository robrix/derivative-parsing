module Main where

import Control.Applicative
import Criterion.Main
import Derivative.Parser

main :: IO ()
main = defaultMain
  [ bench "many/1" $ whnf (parse (parser (many (char 'a')))) (replicate 1 'a')
  , bench "many/10" $ whnf (parse (parser (many (char 'a')))) (replicate 10 'a')
  , bench "many/100" $ whnf (parse (parser (many (char 'a')))) (replicate 100 'a')
  , bench "many/1000" $ whnf (parse (parser (many (char 'a')))) (replicate 1000 'a')
  ]
