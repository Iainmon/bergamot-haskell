module Runner where

import Syntax
import Parser
import Unification

import Data.List (intercalate)

import Data.Map (Map)
import qualified Data.Map as Map

-- Check if a valid unifier can be produced for a term and a given rule
matchRule :: Ord k => Rule k -> Term k -> Maybe (Substitution k)
matchRule (Rule _ c ps) t = if check c t σ then Just σ else Nothing
  where σ = c <~> t

-- Just finds all of the rules that a valid unifier can be created. Doesn't modify anything
match :: Ord k => RuleSystem k -> Term k -> [(Rule k, Substitution k)]
match rs t = [ (r,σ) | r <- rs, Just σ <- [matchRule r t] ]

-- Applies the substitution to the rule
apply :: Ord k => Substitution k -> Rule k -> Rule k
apply σ (Rule n c ps) = Rule n (c <\> σ) (map (<\> σ) ps)

-- Applies the substitution to the rule and its premises
applyMatches :: Ord k => [(Rule k, Substitution k)] -> [(Rule k, Substitution k)]
applyMatches = map (\(r,σ) -> (apply σ r, σ))


ppSubstitution :: Show k => Substitution k -> String
ppSubstitution σ = "{" ++ (intercalate ", " (map (\(k,v) -> show k ++ " -> " ++ show v) (Map.toList σ))) ++ "}"

ppMatch :: Show k => (Rule k, Substitution k) -> String
ppMatch (r,σ) = show r ++ " ~ " ++ ppSubstitution σ

mpIO f xs = mapM_ (putStrLn . f) xs

testMatch rs q = mapM_ (putStrLn . ppMatch) (match rs q)

testRS :: RuleSystem UnificationVar
testRS = [
          parseRule "R1 @ kind(iain) <- ;",
          parseRule "R2 @ virgin(iain) <- ;",
          parseRule "R4 @ kind(kassia) <- ;",
          parseRule "R5 @ virgin(kassia) <- ;",
          parseRule "R8 @ good(?X) <- kind(?X), virgin(?X);"
    ]
-- testRS = [
--           parseRule "BobAlice @ knows(bob,alice) <- ;",
--           parseRule "AliceCarol @ knows(alice,carol) <- ;",
--           parseRule "Transitive @ knows(?X,?Y) <- knows(?X,?Z), knows(?Z,?Y);"
--     ]
r3 :: Rule UnificationVar
r3 = parseRule "R3 @ good(?X) <- kind(?X), virgin(?X);"
testQ :: Term UnificationVar
testQ = (parseQuery "good(iain)")

-- testQ = (parseQuery "knows(bob,carol)")

-- mpIO ppMatch $ match testRS testQ
-- mpIO ppMatch $ applyMatches $ match testRS testQ


-- testRun = runUnifyS (unifyS (parseQuery "good(iain)") (parseQuery "good(?X)")) testRS
testRun = runUnifyS (verifyS (parseQuery "good(?X)")) testRS