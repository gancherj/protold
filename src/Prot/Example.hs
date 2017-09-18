{-# language ExistentialQuantification #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}
{-# language GADTs #-}
module Prot.Example where
import Prot.Lang
import Prot.Exec
import Data.Dynamic
import Data.Parameterized.Some
import Prot.Builder

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
    



        
