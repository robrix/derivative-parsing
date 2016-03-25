{-# LANGUAGE RankNTypes #-}
module Data.Higher.Fix where

import Data.Higher.Functor

newtype HFix f a = F { out :: f (HFix f) a }

-- See http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html for details about the higher-order functionality implemented here.

hcata :: HFunctor h => (forall out. h f out -> f out) -> (forall out. HFix h out -> f out)
hcata algebra = algebra . hfmap (hcata algebra) . out
