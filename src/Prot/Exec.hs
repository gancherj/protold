{-# language ExistentialQuantification #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language UndecidableInstances #-}
{-# language GADTs #-}
module Prot.Exec where
import Data.Parameterized.Some
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Control.Monad.Free
import Prot.Lang
import Data.Typeable
import Data.Dynamic
import System.IO.Unsafe
import qualified Data.Set as Set


runMsg :: [Party] -> Some Msg -> ([Party], Action) 
runMsg [] m = ([], Nothing)
runMsg (p:ps) m = 
    case canReceive p m of
      True ->
          let (p', m') = react p m in
          (p':ps, m')
      False ->
          let (ps', m') = runMsg ps m in
          (p:ps', m')


stepProt :: Some Msg -> State [Party] Action
stepProt m = do
    ps <- get
    let (ps', m') = runMsg ps m
    put ps'
    return m'


allChans :: [Party] -> Set.Set String
allChans ps = foldl (\accum p -> Set.union accum (getChanLabels p)) Set.empty ps

chanSets :: [Party] -> [Set.Set String]
chanSets ps = map getChanLabels ps

countChans :: [Set.Set String] -> String -> Integer
countChans [] c = 0
countChans (s:ss) c =
    if Set.member c s then 1 + countChans ss c else countChans ss c

checkProt :: [Party] -> Bool -- check each input address only has one recipient
checkProt ps = 
    let cs = chanSets ps in
    Set.null $ Set.filter (\c -> countChans cs c > 1) (allChans ps)

runProt :: Chan () -> [Party] -> IO ()
runProt start pi = do
    if not $ checkProt pi then fail "bad pi" else return ()
    putStrLn $ show $ evalState (runProt' (Some (Msg start ()))) pi where
        runProt' :: Some Msg -> State [Party] String
        runProt' m = do
            case m of
              Some (Msg (Chan "stop" StringRep) e) -> return e
              _ -> do
                  m' <- stepProt m
                  case m' of
                    Just m' -> runProt' m'
                    Nothing -> fail "no message produced!"
    

