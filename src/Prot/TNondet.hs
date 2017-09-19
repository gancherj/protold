{-# language DeriveFunctor #-}

module Prot.TNondet (
    TNondet,
    choose, 
    chooseTagged,
    partition)
   where

import Control.Monad
import Control.Monad.State
import qualified Data.List as L

newtype TNondet a = T {deT :: [(a, [Maybe Int])]}
    deriving (Functor, Eq)

instance Show a => Show (TNondet a) where
    show t = show $ deT t

instance Applicative TNondet where
    pure a = T [(a, [Nothing])]
    fm <*> m = T [(f a, c1 ++ c2) | (f, c1) <- deT fm, (a, c2) <- deT m]

instance Monad TNondet where
    return = pure
    fail _ = T []
    d >>= f = T [(y, c1 ++ c2) | (x, c1) <- deT d, (y, c2) <- deT (f x)]

choose :: [a] -> TNondet a
choose as = T $ map (\a -> (a, [Nothing])) as

chooseTagged :: [a] -> TNondet a
chooseTagged as = T $ zipWith (\a i -> (a,[Just i])) as [0..]

partition :: TNondet a -> [[a]]
partition t = map (map fst) xss where
    xss = L.groupBy (\t1 t2 -> (snd t1) == (snd t2)) (deT t)
