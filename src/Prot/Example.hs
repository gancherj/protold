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

acceptB :: Bool -> Chan () -> Chan String -> Chan () -> Chan Bool -> Proc ()
acceptB accB startChan endChan outChan inChan = do
    onInput startChan $ \_ -> output outChan ()

    onInput inChan $ \b ->
        if b == accB then output endChan "good" else output endChan "bad"

prot2' :: Bool -> ProtBuilder
prot2' b = do
    start <- regChan "start" unitRep
    stop <- regChan "stop" stringRep
    atoadv <- regChan "atoadv" unitRep
    advtoa <- regChan "advtoa" boolRep

    regParty (unifAdversaryProc [Some atoadv] [Some advtoa]) () [Some advtoa]
    regParty (acceptB b start stop atoadv advtoa) () [Some stop, Some atoadv]
    return ()

prot2 :: Bool -> [Party]
prot2 b = getProt $ prot2' b 

prot2sim' :: Bool -> ProtBuilder
prot2sim' b = do
    start <- regChan "start" unitRep
    stop <- regChan "stop" stringRep
    atosim <- regChan "atoadv" unitRep
    simtoa <- regChan "advtoa" boolRep

    simtoadv <- regChan "simtoadv" unitRep
    advtosim <- regChan "advtosim" boolRep

    regParty (unifAdversaryProc [Some simtoadv] [Some advtosim]) () [Some advtosim]
    regParty (prot2Sim atosim simtoa simtoadv advtosim) () [Some simtoa, Some simtoadv]
    regParty (acceptB b start stop atosim simtoa) () [Some stop, Some atosim]
    return ()

prot2sim :: Bool -> [Party]
prot2sim b = getProt $ prot2sim' b 

prot2Sim :: Chan () -> Chan Bool -> Chan () -> Chan Bool -> Proc ()
prot2Sim inSim outSim toAdv fromAdv = do
    onInput inSim $ \_ -> output toAdv ()
    onInput fromAdv $ \b -> output outSim (not b)


        
-- how do I program so that the above is painless? "wiring up" adversaries and simulators. I'm imagining a continuation-passing style so that I can pass forward the channels needed for the next stage. It should  be in the above that when I wire up acceptB, I don't need to know in advance who the other party is.
-- The solution is to have ProtBuilder work iteratively, instead of all at once. ie, right now regParty expects all wiring information at once. instead:
--
--  connectIn c p1 
--  connectOut c p2
--
--  means make p1 output on c, and have p2 input on c
--
--  by doing so, I can have values of type (say) pb :: Chan a -> Chan b -> ProtBuilder, which takes an input channel on a, output on b, and constructs a subprotocol which uses them. So I can connect a to pb by
--
-- c1 <- mkChan
-- c2 <- mkChan
-- pb c1 c2
-- connectOut c1 a
-- connectIn c2 a
--
--
