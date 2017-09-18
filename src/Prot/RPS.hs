
module Prot.RPS where
import Prot.Lang

import Data.Parameterized.Some
import Prot.Builder

data RPSPlay = Rock | Paper | Scissors
    deriving (Eq, Show, Typeable)

instance IsEnumerable RPSPlay where
    enumerate = [Rock, Paper, Scissors]

rpsPlayRepr :: Repr RPSPlay
rpsPlayRepr = Repr Enumerable

data RPSCommand = Play RPSPlay | Open
    deriving (Eq, Show, Typeable)

instance IsEnumerable RPSCommand where
    enumerate = (map Play enumerate) ++ [Open]

rpsCommandRepr :: Repr RPSCommand
rpsCommandRepr = Repr Enumerable

data RPSOutcome :: WinL | Tie | WinR
    deriving (Eq, Show, Typeable)

instance IsEnumerable RPSOutcome where
    enumerate = [WinL, Tie, WinR]

rpsOutcomeRepr :: Repr RPSOutcome
rpsOutcomeRepr = Repr Enumerable

rpsWinner :: RPSPlay -> RPSPlay -> RPSOutcome
rpsWinner Rock Scissors = WinL
rpsWinner Paper Rock = WinL
rpsWinner Scissors Paper = WinL
rpsWinner Scissors Rock = WinR
rpsWinner Rock Paper = WinR
rpsWinner Paper Scissors = WinR
rpsWinner _ _ = Tie

honestPlayer :: Chan String -> Chan RPSCommand -> Chan RPSCommand -> Chan RPSOutcome -> Proc ()
player f2me me2f env_input my_output = do
    onInput env_input $ \play ->
        output me2f play
    onInput f2me $ \msg ->
        output my_output msg


fRPS :: Chan RPSCommand -> Chan String -> Chan RPSCommand -> Chan String -> Proc (Maybe RPSPlay, Maybe RPSPlay)
fRPS p1me mep1 p2me mep2 = do
    onInput p1me $ \com -> do
        (playA, playB) <- get
        case (com, playA, playB) of
          (Play p, Nothing, _) ->
              put (Just p, playB)
              output mep1 $ "ok"
          (Open, Just a, Just b) ->
              output mep1 $ show $ rpsWinner a b
          _ ->
              output mep1 "err"
    onInput p2me $ \com -> do
        (playA, playB) <- get
        case (com, playA, playB) of
          (Play p, _, Nothing) ->
              put (playA, Just p)
              output mep2 $ "ok"
          (Open, Just a, Just b) ->
              output mep2 $ show $ rpsWinner a b
          _ ->
              output mep2 "err"



