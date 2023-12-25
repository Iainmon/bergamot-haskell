module Unification where

import Control.Applicative
import Control.Monad
-- import Control.Monad.Unify.Trans hiding (Name)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class ( MonadTrans(lift) ) 

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


import Data.List (intercalate)

import Syntax
import Parser (parseRule, parseQuery)
import Data.Maybe (fromMaybe)

type Substitution k = Map k (Term k)

-- sigma :: Substitution Char
sigma = Map.fromList [('y', Var 'x'), ('x', Var 'a')]
sigma1 = Map.fromList [('y', Var 'x')]
sigma2 = Map.fromList [('x', Var 'a')]

sub :: Ord k => Term k -> Substitution k -> Term k
sub (TQry n ts) σ = TQry n (map (<\> σ) ts)
sub (Var k) σ | Just t <- Map.lookup k σ = t
sub t _ = t

(<\>) :: Ord k => Term k -> Substitution k -> Term k
t <\> σ = sub t σ

(<.>) :: Ord k => Substitution k -> Substitution k -> Substitution k
τ <.> σ = m1 `Map.union` m2
  where m1 = Map.map (\t -> t <\> σ) τ 
        m2 = Map.withoutKeys σ (Map.keysSet τ)  -- Map.filterWithKey (\k _ -> Map.notMember k τ) σ

emptyS :: Substitution k
emptyS = Map.empty

aggregate :: Ord k => [Term k] -> [Term k] -> Substitution k
aggregate [] [] = emptyS
aggregate (t:ts) (s:ss) = u1 <.> (aggregate (map (<\> u1) ts) (map (<\> u1) ss))
  where u1 = t <~> s

(<~>) :: Ord k => Term k -> Term k -> Substitution k
TQry n ts <~> TQry m ss | n == m = aggregate ts ss
TInt i <~> TInt j | i == j = emptyS
TStr s <~> TStr t | s == t = emptyS
Var x <~> Var y | x == y   = emptyS
Var x <~> t | not $ x `elem` freeVars t = Map.singleton x t
t <~> Var x | TQry _ _ <- t, not $ x `elem` freeVars t = Map.singleton x t
t <~> Var x | TInt _ <- t = Map.singleton x t
t <~> Var x | TStr _ <- t = Map.singleton x t
_ <~> _ = emptyS

(<~~>) :: Ord k => Term k -> Term k -> Substitution k
(<~~>) = flip (<~>)

check :: Ord k => Term k -> Term k -> Substitution k -> Bool
check t1 t2 σ = (t1 <\> σ) == (t2 <\> σ)

unify :: Ord k => Term k -> Term k -> Maybe (Substitution k)
unify t1 t2 = if check t1 t2 σ then Just σ else Nothing
  where σ = t1 <~> t2


data UnifyState k 
  = MkUnifyState { substitution :: Substitution k
                 , counter :: Int
                 , rules :: RuleSystem k
                 } deriving (Eq, Ord, Show)

emptyUS :: UnifyState k
emptyUS = MkUnifyState emptyS 0 []


type UnifyS k a = StateT (UnifyState k) [] a


unifyS :: Ord k => Term k -> Term k -> UnifyS k (Substitution k)
unifyS t1 t2 = do
  σ <- gets substitution
  let mσ' = unify (t1 <\> σ) (t2 <\> σ)
  σ' <- fromMaybe mzero (fmap return mσ')
  let σ'' = σ <.> σ'
  modify (\s -> s { substitution = σ'' })
  return σ''

updateS :: Ord k => Substitution k -> UnifyS k ()
updateS σ' = do
  σ <- gets substitution
  modify (\s -> s { substitution = σ' <.> σ })

applyS :: Ord k => Term k -> UnifyS k (Rule k)
applyS t = do
  rs <- gets rules
  r <- lift rs
  unifyS (conclusion r) t
  return r



verifyS :: Ord k => Term k -> UnifyS k ()
verifyS t = do
  r <- applyS t
  sequence_ $ map verifyS (premises r)

  -- ps <- map (<\> σ) <$> mapM applyS (premises r)
  -- mapM_ verifyS ps
  -- updateS (c <~> (conclusion r))

-- runUnifyS :: Ord k => UnifyS k a -> RuleSystem k -> [(a, Substitution k)]
runUnifyS :: Ord k => UnifyS k a -> RuleSystem k -> [(a, UnifyState k)]
runUnifyS m rs = runStateT m (MkUnifyState emptyS 0 rs)







t1 = (TQry "f" [Var 'x'])
t2 = (TQry "f" [TInt 1])
-- KB is just a set of rules, (Term MetaVar, [Term MetaVar], Name)
-- 1) Unify query `Q(t1,...,tn)` with each conclusion `C(M1,...,Mn)` in KB, producing a substitution σ.
--       - That is, get subsitutions for each conclusion and apply the valid ones to each conclusion.
--       - If all of the conclusions are not unifiable, then the query is not unifiable.
-- 2) Substitute σ into `C(M1,...,Mn) -> C(t1,...,tn)` and each premise `Pn(M1,...,Mn) -> Pn(t1,...,tn)` of the rule, and recursively call the algorithm on each premise.


{--
knows(bob,alice).
knows(alice,carol).
knows(X,Y) :- knows(X,Z), knows(Z,Y).

?- knows(bob,carol).

knows(bob,carol) <~> knows(X,Y) => { X -> bob,  Y -> carol }
knows(bob,carol) :- knows(bob,Z), knows(Z,carol
--}

cl s ps = TQry s ps
vr = Var 

knows = cl "knows"
alice = TStr "alice"
bob = TStr "bob"
carol = TStr "carol"
u = (knows [bob,carol]) <~~> (knows [vr 'X',vr 'Y'])

-- f(x,g(y))
exp1 :: Term Char
exp1 = TQry "f" [Var  'x', TQry "g" [Var 'y']]

-- f(g(z),w)
exp2 :: Term Char
exp2 = TQry "f" [TQry "g" [Var 'z'], Var 'w']






-- good(X) :- kind(X), virgin(X).


{--
{ } => good(iain) ~ good(X) { X -> iain }

{ X -> 2 } => X is 2
{ } => X ~ 42 => { X -> 42 }
{ X -> 42 } => 
--}

{--
R1 @ kind(iain).
R2 @ virgin(iain).
R3 @ good(X) :- kind(X), virgin(X).

?- good(iain).

good(iain) <~> good(X) => { X -> iain }
{ X -> iain } kind(X)
kind(X) <\> { X -> iain } <~> kind(iain)
--}

merge :: Ord k => Term k -> Term k -> Substitution k -> Maybe (Substitution k)
merge t1 t2 σ = if check t1' t2' σ' then Just σ' else Nothing
  where σ' = t1' <~> t2'
        t1' = t1 <\> σ
        t2' = t2 <\> σ

step :: Ord k => Term k -> Term k -> Substitution k -> Maybe (Substitution k)
step t1 t2 σ = if check t1' t2 σ' then Just (σ' <.> σ) else Nothing
  where σ' = t1' <~> t2
        t1' = t1 <\> σ

applyRule :: Ord k => Term k -> Rule k -> Substitution k -> Maybe (Substitution k, [Term k])
applyRule t (Rule _ c ps) σ | Just σ' <- merge c t σ = Just (σ',ps)
applyRule _ _ _ = Nothing


-- prove :: Ord k => Substitution k -> Term k -> Rule k -> 
-- -- prove σ t (Rule _ c []) | check c t σ then Just σ else Nothing
-- prove σ t (Rule _ c ps) = 
--   where σ' = c <~> (t <\> σ)

