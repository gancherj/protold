{-# language ExistentialQuantification #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language StandaloneDeriving #-}
{-# language RankNTypes #-}
{-# language GADTs #-}
module Prot.Builder where
import Prot.LangNew
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

regParty :: Proc s -> s -> [Some Chan] -> State BuilderState Party -- third argument is the list of channels it outputs on (only necessary for analysis)
regParty a b c = do
    parties <- use curParties
    curParties .= (procToParty a b c) : parties
    return (procToParty a b c)


getProt :: State BuilderState a -> [Party]
getProt s = _curParties $ execState s (BuilderState [] [])

