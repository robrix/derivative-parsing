module Main where

import Control.Applicative
import Derivative.Parser
import Weigh

main :: IO ()
main = mainWith $ do
  func "many/1" (parse (parser (many (char 'a')))) (replicate (10 ^ 0) 'a')
