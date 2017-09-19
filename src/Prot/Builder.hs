{-# language ExistentialQuantification #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language StandaloneDeriving #-}
{-# language RankNTypes #-}
{-# language GADTs #-}
module Prot.Builder where
import Prot.Lang
import Prot.Analyze
import Control.Monad
import Control.Monad.State
import Data.Parameterized.Some
import Control.Lens hiding (op)

data BuilderState = BuilderState {
    _curChans :: [Some Chan],
    _curParties :: [Party]
}

type ProtBuilder =  State BuilderState ()

curChans :: Simple Lens (BuilderState) ([Some Chan])
curChans = lens _curChans (\s v -> s { _curChans = v })

curParties :: Simple Lens (BuilderState) ([Party])
curParties = lens _curParties (\s v -> s { _curParties = v })

regChan :: String -> Repr a -> State BuilderState (Chan a)
regChan s rep = do
    chans <- use curChans
    curChans .= Some (Chan s rep):chans
    return (Chan s rep)

regParty :: Proc s -> s -> [Some Chan] -> State BuilderState () -- third argument is the list of channels it outputs on (only necessary for analysis)
regParty proc s outs = do
    parties <- use curParties
    curParties .= (procToParty proc s outs) : parties
    return ()


getProt :: State BuilderState a -> [Party]
getProt s = _curParties $ execState s (BuilderState [] [])

mkParty :: s -> [Some Chan] -> [Some Chan] -> Proc s -> ProtBuilder
mkParty s ins outs p = regParty p s outs

unifProt :: [Some Chan] -> [Some Chan] -> ProtBuilder
unifProt ins outs = mkParty () ins outs (unifAdversaryProc ins outs)

echoProt :: Int -> [(Some Chan, Some Chan)] -> NondetState Int Action -> ProtBuilder
echoProt i pairs act = mkParty 0 (map fst pairs) (map snd pairs) (boundedEchoProc i pairs act)



