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
import Prot.Builder
import Prot.Analyze
import Prot.RPS

    
main = do
    -- putStrLn $ show $ runProt $ getProt $ (prot2Real True )
    -- putStrLn $ show $ identicalProts (getProt $ prot2Real True) (getProt $ prot2Ideal False)
    putStrLn $ show $  runProt $ getProt $  rpsIdealBoundedEnv
    
    putStrLn "hello world"
