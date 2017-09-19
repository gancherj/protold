
module Prot.RPS where
import Prot.Lang
import Control.Monad
import Control.Monad.State
import Data.Typeable
import Data.Parameterized.Some
import Prot.Builder

data RPSPlay = Rock | Paper | Scissors
    deriving (Eq, Show, Typeable)

instance IsEnumerable RPSPlay where
    enumerate = [Rock, Paper, Scissors]

rpsPlayRepr :: Repr RPSPlay
rpsPlayRepr = EnumerableRepr Enumerable

data RPSCommand = Play RPSPlay | Open
    deriving (Eq, Show, Typeable)

instance IsEnumerable RPSCommand where
    enumerate = (map Play enumerate) ++ [Open]

rpsCommandRepr :: Repr RPSCommand
rpsCommandRepr = EnumerableRepr Enumerable

data RPSOutcome = Ok | Err | WinL | Tie | WinR
    deriving (Eq, Show, Typeable)

instance IsEnumerable RPSOutcome where
    enumerate = [WinL, Tie, WinR]

rpsOutcomeRepr :: Repr RPSOutcome
rpsOutcomeRepr = EnumerableRepr Enumerable

rpsWinner :: RPSPlay -> RPSPlay -> RPSOutcome
rpsWinner Rock Scissors = WinL
rpsWinner Paper Rock = WinL
rpsWinner Scissors Paper = WinL
rpsWinner Scissors Rock = WinR
rpsWinner Rock Paper = WinR
rpsWinner Paper Scissors = WinR
rpsWinner _ _ = Tie

type RPSIdealPlayerType = Chan RPSOutcome  -> Chan RPSCommand -> Chan RPSCommand -> Chan RPSOutcome -> ProtBuilder
type RPSIdealEnvType = Chan () -> Chan Bool -> Chan RPSOutcome  -> Chan RPSCommand -> Chan RPSOutcome -> Chan RPSCommand -> ProtBuilder

honestPlayerIdeal :: Chan RPSOutcome  -> Chan RPSCommand -> Chan RPSCommand -> Chan RPSOutcome -> ProtBuilder
honestPlayerIdeal f2me me2f env_input my_output = mkParty () [Some f2me, Some env_input] [Some me2f, Some my_output] $ do
    onInput env_input $ \play ->
        output me2f play
    onInput f2me $ \msg ->
        output my_output msg


fRPS :: Chan RPSCommand -> Chan RPSOutcome -> Chan RPSCommand -> Chan RPSOutcome -> ProtBuilder 
fRPS p1me mep1 p2me mep2 = mkParty (Nothing, Nothing) [Some p1me, Some p2me] [Some mep1, Some mep2] $ do
    onInput p1me $ \com -> do
        (playA, playB) <- get
        case (com, playA, playB) of
          (Play p, Nothing, _) -> do
              put (Just p, playB)
              output mep1 $ Ok
          (Open, Just a, Just b) -> do
              output mep1 $  rpsWinner a b
          _ -> do
              output mep1 Err
    onInput p2me $ \com -> do
        (playA, playB) <- get
        case (com, playA, playB) of
          (Play p, _, Nothing) -> do
              put (playA, Just p)
              output mep2 $ Ok
          (Open, Just a, Just b) ->
              output mep2 $  rpsWinner a b
          _ ->
              output mep2 Err


rpsIdeal :: RPSIdealPlayerType -> RPSIdealPlayerType -> RPSIdealEnvType -> ProtBuilder
rpsIdeal p1 p2 env = do
    start <- regChan "start" unitRep
    stop <- regChan "stop" boolRep
    env2p1 <- regChan "env2p1" rpsCommandRepr
    p12env <- regChan "p12env" rpsOutcomeRepr
    env2p2 <- regChan "env2p2" rpsCommandRepr
    p22env <- regChan "p22env" rpsOutcomeRepr

    p12f <- regChan "p12f" rpsCommandRepr
    p22f <- regChan "p22f" rpsCommandRepr
    f2p1 <- regChan "f2p1" rpsOutcomeRepr
    f2p2 <- regChan "f2p2" rpsOutcomeRepr

    fRPS p12f f2p1 p22f f2p2
    p1 f2p1 p12f env2p1 p12env
    p2 f2p2 p22f env2p2 p22env
    env start stop p12env env2p1 p22env env2p2 
    return ()
    
rpsIdealAllAdv = rpsIdeal padv padv eadv where -- this loops infinitely
    padv = (\a b c d -> unifProt [Some a, Some c] [Some b, Some d])
    eadv = (\a b c d e f -> unifProt [Some a, Some c, Some e] [Some b, Some d, Some f])
    
rpsIdealBoundedEnv = rpsIdeal honestPlayerIdeal padv (boundedEnvIdealBuild (5) eadv) where
    padv = (\a b c d -> unifProt [Some a, Some c] [Some b, Some d])
    eadv = (\a b c d e f -> unifProt [Some a, Some c, Some e] [Some b, Some d, Some f])


boundedEnvIdealBuild :: Int -> RPSIdealEnvType -> RPSIdealEnvType
boundedEnvIdealBuild i env = \a b c d e f -> do
    enva <- regChan "enva" unitRep
    envb <- regChan "envb" boolRep
    envc <- regChan "envc" rpsOutcomeRepr
    envd <- regChan "envd" rpsCommandRepr
    enve <- regChan "enve" rpsOutcomeRepr
    envf <- regChan "envd" rpsCommandRepr
    env enva envb envc envd enve envf
    echoProt i [(Some a, Some enva), (Some envb, Some b), (Some c, Some envc), (Some envd, Some d), (Some e, Some enve), (Some envf, Some f)] (output b False)

------
--


-- honestPlayerReal :: Chan 
