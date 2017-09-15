
module Prot.KVList where

type KVList k v = [(k,v)]

kvEmpty :: KVList k v
kvEmpty = []

kvGet :: Eq k => k -> KVList k v -> Maybe v
kvGet k [] = Nothing
kvGet k ((k',v):ks) = if k == k' then (Just v) else kvGet k ks

kvInsert :: Eq k => k -> v -> KVList k v -> KVList k v
kvInsert k v ks = (k,v):ks

kvIn :: Eq k => k -> KVList k v -> Bool
kvIn k [] = False
kvIn k ((k',_):ks) = if k == k' then True else kvIn k ks
