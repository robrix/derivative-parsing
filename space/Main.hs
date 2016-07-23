module Main where

import Control.Applicative
import Control.DeepSeq
import Derivative.Parser
import Weigh

main :: IO ()
main = mainWith $ do
  let p = parser (many (char 'a'))
  func "many/1" (parse p) (replicate (10 ^ 0) 'a')

wbench :: NFData a => String -> Weighable a b -> Weigh ()
wbench name w = runWeighable w (func name)

newtype Weighable a b = Weighable { runWeighable :: ((b -> a) -> b -> Weigh ()) -> Weigh () }

weigh :: NFData a => (b -> a) -> b -> Weighable a b
weigh f a = Weighable $ \ g -> g f a
