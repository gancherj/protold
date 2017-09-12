{-# language ExistentialQuantification #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language StandaloneDeriving #-}
{-# language GADTs #-}
module Prot.Lang where
import Data.Parameterized.Some
import Data.Parameterized.Classes as C
import Control.Monad.State
import Control.Monad.Free
import Data.Dynamic
import Data.Type.Equality
import qualified Data.Set as Set


data Repr a where
    IntRep :: Repr Int
    StringRep :: Repr String
    BoolRep :: Repr Bool
    UnitRep :: Repr ()

deriving instance Show (Repr tp)


instance C.ShowF Repr

instance TestEquality Repr where
    IntRep `testEquality` IntRep = Just Refl
    StringRep `testEquality` StringRep = Just Refl
    BoolRep `testEquality` BoolRep = Just Refl
    UnitRep `testEquality` UnitRep = Just Refl
    testEquality _ _ = Nothing


data Chan a = Chan String (Repr a)

type Action = Maybe (Some Msg)
data Reaction s a  = Reaction (Chan a) (a -> State s Action)
data Msg a = Msg (Chan a) a

data ProcF s k where
    OnInput :: Chan a -> (a -> State s Action) -> k -> ProcF s k 
    End :: ProcF s k

instance Functor (ProcF s) where
    fmap f End = End
    fmap f (OnInput a b c) = OnInput a b (f c)

type Proc s = Free (ProcF s) ()

onInput :: Chan a -> (a -> State s Action) -> Proc s
onInput chan act = liftF (OnInput chan act ())

endProc :: Proc s
endProc = liftF End


output :: Chan a -> a -> State s Action
output chan val = return $ Just $ Some $ Msg chan val

pass :: State s Action
pass = return Nothing

data Party = forall s. Party {
    pState :: s, 
    pActlist :: [Some (Reaction s)]
 }

procToList :: Proc s -> [Some (Reaction s)]
procToList (Free (OnInput c f k)) = (Some $ Reaction c f) : (procToList k)
procToList _ = []

procToParty :: Proc s -> s -> Party
procToParty p s = Party s (procToList p) 

canReceive :: Party -> Some Msg -> Bool
canReceive (Party s as) (Some m) =
    case findReaction as m of
      Just _ -> True
      Nothing -> False

findReaction :: [Some (Reaction s)] -> Msg a -> Maybe (Reaction s a)
findReaction [] _ = Nothing
findReaction (r:rs) m = 
    case (r,m) of
      (Some (Reaction (Chan c repr) fn), Msg (Chan c' repr') _) ->
          case (testEquality repr repr', c == c') of
            (Just Refl, True) ->
                Just (Reaction (Chan c repr) fn)
            _ -> findReaction rs m


react :: Party -> Some Msg -> (Party, Action)
react (Party s as) (Some msg) =
    case (findReaction as msg, msg) of
      (Just (Reaction _ fn), Msg _ m)  ->
          let (a', s') = runState (fn m) s in
          (Party s' as, a')


getChanLabels :: Party -> Set.Set String
getChanLabels (Party s as) = go as where
    go [] = Set.empty
    go (a : as) = 
        case a of
          Some (Reaction (Chan c _) _) ->
              Set.insert c (go as)
