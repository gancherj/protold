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
import qualified Prot.LangNew as L

    
main = do
    runProt $ getProt prot 
    putStrLn "hello world"
