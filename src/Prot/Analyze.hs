{-# language ExistentialQuantification #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}
{-# language GADTs #-}
module Prot.Analyze where
import Prot.Lang
import qualified Prot.Exec as Exec
import Data.Parameterized.Some
import Prot.KVList
import qualified Data.List as L
import Control.Monad
import Prot.Exec
import Control.Monad.State

-- an adversary is denoted to be an arbitrary party whose random coins are fixed. This means that an adversary is an arbitrary-but-fixed function from traces to messages. No coherency condition is required on this adversary.
-- 
-- a trace for a single party is a list of (in message, out message) pairs
--
--

unifAdversaryProc :: [Some Chan] -> [Some Chan] -> Proc ()
unifAdversaryProc ins outs = 
    forM_ ins 
        (\(Some inc) -> onInput inc $ \_ -> do
            outm <- chooseTagged $ allActions outs
            outputMsg outm)


-- given a collection of 

-- given all output channels, enumerate what messages I can send
allActions :: [Some Chan] -> [Some Msg]
allActions scs = concat $ map
    (\sc -> case sc of
      Some chan@(Chan c (EnumerableRepr Enumerable)) ->
          map (\e -> Some (Msg chan e)) enumerate
      _ -> fail "not enumerable!") scs


removeBy :: (a -> Bool) -> [a] -> Either [a] [a]
removeBy f [] = Right []
removeBy f (x:xs) = 
    if f x then Left xs else 
        case (removeBy f xs) of
          Left xs' -> Left (x:xs)
          Right xs' -> Right (x:xs)

isPermutationBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
isPermutationBy f [] [] = True
isPermutationBy f (x:xs) [] = False
isPermutationBy f [] (y:ys) = False
isPermutationBy f (x:xs) ys =
    case removeBy (f x) ys of
      Left ys' -> 
          isPermutationBy f xs ys'
      Right _ -> False

isDoublePerm :: Eq a => [[a]] -> [[a]] -> Bool
isDoublePerm = isPermutationBy (isPermutationBy (==))

identicalProts :: [Party] -> [Party] -> Bool
identicalProts a b = isDoublePerm (runProt a) (runProt b) -- TODO is this correct??

