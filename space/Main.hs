module Main where

import Control.Applicative
import Derivative.Parser
import Weigh

main :: IO ()
main = mainWith $ do
  let p = parser (many (char 'a'))
  func "many/1" (parse p) (replicate (10 ^ 0) 'a')
