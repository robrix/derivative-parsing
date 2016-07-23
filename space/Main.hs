module Main where

import Control.Applicative
import Control.DeepSeq
import Derivative.Parser
import Weigh

main :: IO ()
main = mainWith $ do
  wgroup "many" $ let p = parser (many (char 'a'))
                      b n = weigh (show n) (parse p) (replicate n 'a') in
    [ b 1
    ]

wgroup :: NFData a => String -> [Weighable a b] -> Weigh ()
wgroup prefix = fmap (() <$) . traverse $ \ w ->
  runWeighable w (\ name -> func (prefix ++ "/" ++ name))

newtype Weighable a b = Weighable { runWeighable :: (String -> (b -> a) -> b -> Weigh ()) -> Weigh () }

weigh :: String -> (b -> a) -> b -> Weighable a b
weigh s f a = Weighable $ \ g -> g s f a
