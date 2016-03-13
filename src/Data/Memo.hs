module Data.Memo
(
) where

insert :: key -> value -> Table key value -> [(key, value)]
insert key value = ((key, value) :)
