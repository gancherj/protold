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

    
main = do
    putStrLn $ show $ runProt $ (prot2 True )
    putStrLn $ show $ runProt $ (prot2sim False )
    putStrLn "hello world"
