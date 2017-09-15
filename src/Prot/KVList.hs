
module Prot.KVList where

type KVList k v = [(k,v)]

kvEmpty :: KVList k v
kvEmpty = []

kvLookup :: Eq k => k -> KVList k v -> Maybe v
kvLookup k [] = Nothing
kvLookup k ((k',v):ks) = if k == k' then (Just v) else kvLookup k ks

-- assumes k is not in ks
kvInsert :: Eq k => k -> v -> KVList k v -> KVList k v
kvInsert k v ks = (k,v):ks

kvIn :: Eq k => k -> KVList k v -> Bool
kvIn k [] = False
kvIn k ((k',_):ks) = if k == k' then True else kvIn k ks

-- lookup k in the list. if it does not exist, use the function provided to make a v. return the v found or created, along with the updated list
kvGet :: Eq k => k -> KVList k v -> (k -> v) -> (KVList k v, v)
kvGet k ks f =
    case kvLookup k ks of
      Just v -> (ks, v)
      Nothing -> let v = f k in (kvInsert k v ks, v)

