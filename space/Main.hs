module Main where

import Control.Applicative
import Control.DeepSeq
import Derivative.Parser
import Weigh

main :: IO ()
main = mainWith $ do
  let p = parser (many (char 'a'))
  func "many/1" (parse p) (replicate (10 ^ 0) 'a')

wgroup :: NFData a => String -> [Weighable a b] -> Weigh ()
wgroup prefix = fmap (() <$) . traverse $ \ w ->
  runWeighable w (\ name -> func (prefix ++ "/" ++ name))

newtype Weighable a b = Weighable { runWeighable :: (String -> (b -> a) -> b -> Weigh ()) -> Weigh () }

weigh :: NFData a => String -> (b -> a) -> b -> Weighable a b
weigh s f a = Weighable $ \ g -> g s f a
