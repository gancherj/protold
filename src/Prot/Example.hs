{-# language ExistentialQuantification #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}
{-# language GADTs #-}
module Prot.Example where
import Prot.Lang
import Prot.Exec
import Control.Monad
import Data.Dynamic
import Data.Parameterized.Some
import Prot.Builder
import Prot.Analyze

ping :: Chan () -> Chan Int -> Chan Int -> Proc ()
ping ex_start outc inc = do
    onInput ex_start $ \_ -> do
        output outc 0
    onInput inc $ \j -> do
        i <- choose [1,2]
        output outc (j + i)

pong :: Chan String -> Chan Int -> Chan Int -> Proc ()
pong ex_end outc inc = do
    onInput inc $ \j ->
        if j > 10 then
            output ex_end $ "hello: " ++ (show j)
        else
            output outc (j + 1)

prot1 :: ProtBuilder
prot1 = do
    start <- regChan "start" unitRep
    a <- regChan "pingpong" intRep
    b <- regChan "pongping" intRep
    stop <- regChan "stop" stringRep
    
    regParty (ping start a b) () [Some a]
    regParty (pong stop b a) () [Some stop, Some b]
    return ()
    

----------
--


acceptB :: Bool -> Chan () -> Chan String -> Chan () -> Chan Bool -> ProtBuilder
acceptB accB startChan endChan outChan inChan = mkParty () [Some startChan, Some inChan] [Some outChan,Some endChan] $
    do
    onInput startChan $ \_ -> output outChan ()

    onInput inChan $ \b ->
        if b == accB then output endChan "good" else output endChan "bad"


prot2' :: Bool -> (Chan () -> Chan Bool -> ProtBuilder) -> ProtBuilder
prot2' b adv = do
    start <- regChan "start" unitRep
    stop <- regChan "stop" stringRep
    toadv <- regChan "atoadv" unitRep
    fromadv <- regChan "advtoa" boolRep
    adv toadv fromadv
    acceptB b start stop toadv fromadv
    return ()

prot2Real :: Bool -> ProtBuilder
prot2Real b = prot2' b (\advin advout -> unifProt [Some advin] [Some advout])

prot2Ideal :: Bool -> ProtBuilder
prot2Ideal b = prot2' b prot2sim

prot2sim :: Chan () -> Chan Bool -> ProtBuilder
prot2sim simin simout = do

    simtoadv <- regChan "simtoadv" unitRep
    advtosim <- regChan "advtosim" boolRep

    unifProt [Some simtoadv] [Some advtosim]
    prot2Sim simin simout simtoadv advtosim
    return ()


prot2Sim :: Chan () -> Chan Bool -> Chan () -> Chan Bool -> ProtBuilder
prot2Sim inSim outSim toAdv fromAdv = mkParty () [Some inSim, Some fromAdv] [Some outSim, Some toAdv] $
    do
    onInput inSim $ \_ -> output toAdv ()
    onInput fromAdv $ \b -> output outSim (not b)


        
-- current solution: parties are written as elements of ProtBuilder. Then adversaries are higher-order elements of protbuilder, exporting an interface
