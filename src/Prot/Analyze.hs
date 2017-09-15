{-# language ExistentialQuantification #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}
{-# language GADTs #-}
module Prot.Analyze where
import Prot.Lang 
import qualified Prot.Exec as Exec
import Data.Parameterized.Some
import Prot.Builder
import Prot.KVList
import Control.Monad
import Control.Monad.State

data SymParty = Honest Party | Adv [Some Chan] [Some Chan] Trace

-- given all output channels, enumerate what messages I can send
allActions :: [Some Chan] -> [Some Msg]
allActions scs = concat $ map
    (\sc -> case sc of
      Some chan@(Chan c (EnumerableRepr Enumerable)) ->
          map (\e -> Some (Msg chan e)) enumerate
      _ -> fail "not enumerable!") scs

-- a trace for a single party is a list of (in message, out message) pairs
type Trace = KVList (Some Msg) (Some Msg)

emptyTrace :: Trace
emptyTrace = kvEmpty

extendTrace :: Some Msg -> -- input message
               [Some Chan] -> -- possible output channels
               Trace -> -- trace so far
               [Trace] -- all new traces
extendTrace msg@(Some (Msg chan _)) outputChans tr = map (\msg' -> kvInsert msg msg' tr) (allActions outputChans)

                  
-- idea: if I want to compare an adversary across two executions with the same random coins, I lazily extend their (nondeterministic) trace, and make the two executions agree with each other by using the same trace in both nondeterministic executions.
--

type NondetState s a = StateT s [] a

symCanReceive :: SymParty -> Some Msg -> Bool
symCanReceive (Honest p) m = canReceive p m
symCanReceive (Adv ins outs tr) m = chansCanReceive ins m


symReact :: SymParty -> Some Msg -> [(Party, Action)]
symReact = error "unimp"

{- runMsg :: [SymParty] -> Some Msg -> NondetState [SymParty] Action
runMsg [] m = return Nothing
runMsg (sp:sps) m =
    case symCanReceive sp m of
      True -> do
          (sp', m') <- symReact sp m
TODO implement -}



