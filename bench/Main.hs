module Main where

import Control.Applicative
import Criterion.Main
import Derivative.Parser

main :: IO ()
main = defaultMain
  [ bgroup "many" $ let p = parser (many (char 'a'))
                        b n = bench (show n) $ whnf (parse p) (replicate n 'a') in
    b . (10 ^) <$> [0..4]
  ]
