{-# LANGUAGE GADTs #-}
module Derivative.Parser where

data Parser a where
  Ret :: [a] -> Parser a
  Nul :: Parser a
  Eps :: Parser a
