{-# LANGUAGE RankNTypes #-}
module Data.Graph where

data Rec f v
  = Var v
  | Mu ([v] -> [f (Rec f v)])
  | In (f (Rec f v))

newtype Graph f = Down { up :: forall v. Rec f v }

gfold :: Functor f => (t -> c) -> (([t] -> [c]) -> c) -> (f c -> c) -> Graph f -> c
gfold var bind recur = trans . up
  where trans (Var v) = var v
        trans (Mu g) = bind (map (recur . fmap trans) . g)
        trans (In fa) = recur (fmap trans fa)
