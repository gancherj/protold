{-# language ExistentialQuantification #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}
{-# language GADTs #-}
module Prot.Analyze where
import Prot.Lang 
import Prot.Exec
import Data.Parameterized.Some
import Prot.Builder

data SymParty = Honest Party | Adv [Some Chan] [Some Chan]

allActions :: [Some Chan] -> [Some Msg]
allActions outChans = concat $ map 
    (\sc -> case sc of
              Some chan@(Chan c (EnumerableRepr Enumerable)) ->
                  map (\e -> Some (Msg chan e)) enumerate
              _ -> fail "not enumerable!") outChans



                  


