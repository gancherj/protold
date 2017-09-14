
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
import Data.Typeable
import qualified Data.Set as Set

intRep :: Repr Int
intRep = SymbolicRepr Symbolic

stringRep :: Repr String
stringRep = SymbolicRepr Symbolic

unitRep :: Repr ()
unitRep = EnumerableRepr Enumerable

instance IsEnumerable () where
    enumerate = [()]


class IsEnumerable a where
    enumerate :: [a]

data Symbolic a where
    Symbolic :: (Eq a, Show a, Typeable a) => Symbolic a

instance Show (Symbolic a) where
    show a = case a of
               b@Symbolic -> show (typeOf b)

instance TestEquality Symbolic where
    testEquality Symbolic Symbolic = eqT

data Enumerable a where
    Enumerable :: (Eq a, Show a, Typeable a, IsEnumerable a) => Enumerable a

instance Show (Enumerable a) where
    show a = case a of
               b@Enumerable -> show (typeOf b)

instance TestEquality Enumerable where
    testEquality Enumerable Enumerable = eqT

data Repr a where
    SymbolicRepr :: Symbolic a -> Repr a
    EnumerableRepr :: Enumerable a -> Repr a

deriving instance Show (Repr tp)


instance TestEquality Repr where
    testEquality (SymbolicRepr a) (SymbolicRepr b) = testEquality a b
    testEquality (EnumerableRepr a) (EnumerableRepr b) = testEquality a b
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
    pActlist :: [Some (Reaction s)],
    pChansIn :: [Some Chan],
    pChansOut :: [Some Chan]
 }

procToList :: Proc s -> [Some (Reaction s)]
procToList (Free (OnInput c f k)) = (Some $ Reaction c f) : (procToList k)
procToList _ = []

procToParty :: Proc s -> s -> [Some Chan] -> Party
procToParty p s outs = 
    Party s (procToList p) 
        (map (\pr -> case pr of Some (Reaction ch _) -> Some ch) (procToList p)) outs

canReceive :: Party -> Some Msg -> Bool
canReceive (Party s as a b) (Some m) =
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
react (Party s as a b) (Some msg) =
    case (findReaction as msg, msg) of
      (Just (Reaction _ fn), Msg _ m)  ->
          let (a', s') = runState (fn m) s in
          (Party s' as a b, a')


getChanLabels :: Party -> Set.Set String
getChanLabels (Party s as _ _) = go as where
    go [] = Set.empty
    go (a : as) = 
        case a of
          Some (Reaction (Chan c _) _) ->
              Set.insert c (go as)
