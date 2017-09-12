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

ping :: Chan () -> Chan Int -> Chan Int -> Proc ()
ping ex_start outc inc = do
    onInput ex_start $ \_ -> do
        output outc 0
    onInput inc $ \j -> do
        output outc (j + 1)

pong :: Chan String -> Chan Int -> Chan Int -> Proc ()
pong ex_end outc inc = do
    onInput inc $ \j ->
        if j > 10 then
            output ex_end "hello"
        else
            output outc (j + 1)

startchan = Chan "start" UnitRep
pingpong = Chan "pingpong" IntRep
pongping = Chan "pongping" IntRep
stop = Chan "stop" StringRep

pingInst = ping startchan pingpong pongping
pongInst = pong stop pongping pingpong

pingP = procToParty pingInst ()
pongP = procToParty pongInst ()
        
