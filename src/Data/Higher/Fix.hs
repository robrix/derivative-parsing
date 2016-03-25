module Data.Higher.Fix where

newtype HFix f a = F { out :: f (HFix f) a }
