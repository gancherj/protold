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
import Data.Map.Lazy
import Prot.Builder
import Prot.KVList
import Control.Monad
import Control.Monad.State

-- an adversary is denoted to be an arbitrary party whose random coins are fixed. This means that an adversary is an arbitrary-but-fixed function from traces to messages. No coherency condition is required on this adversary.
-- 
-- a trace for a single party is a list of (in message, out message) pairs
--
data SymParty = Honest Party | Adv [Some Chan] [Some Chan] String

data ExecTree = ExecTree [(Some Msg, Some Msg, ExecTree)] 

type AdvMap = Map.Map String ExecTree

-- goal: given a protocol (ie, [SymParty]), execute it by following each party exactly, and exhaustively computing all possible adversary reactions. This will result in a data structure called the AdvMap, which maps adversary IDs to their execution trees. Given an AdvMap, we do the following: for each choice paths in the AdvMap, we execute both protocols fully. This generates a list of pairs [(protocol output 1, protocol output 2)]


-- given all output channels, enumerate what messages I can send
allActions :: [Some Chan] -> [Some Msg]
allActions scs = concat $ map
    (\sc -> case sc of
      Some chan@(Chan c (EnumerableRepr Enumerable)) ->
          map (\e -> Some (Msg chan e)) enumerate
      _ -> fail "not enumerable!") scs

type Trace = [(Some Msg, Some Msg])


getActionFollowing :: Trace ->
                      Some Msg ->
                      

-- assumes advHistory doesn't already have something of the form (inMsg, outmsg) : Trace; if it did, I would just use that
extendTrace :: Trace -> 
             Some Msg ->
             [Some Chan] ->
             [Trace]

extendTrace curTrace inMsg outChans =
    map (\msg -> kvInsert inMsg msg curTrace) (allActions outChans)


{-
extendTrace :: Some Msg -> -- input message
               [Some Chan] -> -- possible output channels
               Trace -> -- trace so far
               [Trace] -- all new traces
extendTrace msg@(Some (Msg chan _)) outputChans tr = map (\msg' -> kvInsert msg msg' tr) (allActions outputChans)
-}
                  
-- idea: if I want to compare an adversary across two executions with the same random coins, I lazily extend their (nondeterministic) trace, and make the two executions agree with each other by using the same trace in both nondeterministic executions.
--
{-
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


-}
