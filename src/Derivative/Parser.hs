{-# LANGUAGE GADTs #-}
module Derivative.Parser where

data Parser a where
  Nul :: Parser a
