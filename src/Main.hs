{-# language ExistentialQuantification #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language GADTs #-}
module Main where
import Data.Parameterized.Some
import Control.Monad.State
import Control.Monad.Free
import Prot.Lang
import Prot.Exec
import Prot.Example

    
main = do
    runProt startchan [pingP, pongP]
    putStrLn "hello world"
