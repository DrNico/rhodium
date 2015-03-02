
module Rhodium.Context where

import Control.Monad.Error.Class

import Prelude hiding ((.), id)

data Term var
  = Var var
  | Pred String [Term var]
  deriving (Eq,Show)

data ObC  = ObC [Term Int] deriving (Eq)
data HomC = HomC {
        source      :: [Term Int],
        target      :: [Term Int],
        morph       :: [Term Int]
    }
    deriving (Eq)

---- Dependent projection
ft :: ObC -> ObC
ft (ObC ob) = ObC $ tail ob

-- Pre-condition Ob_n , n > 0
proj :: ObC -> HomC
proj (ObC obs) = HomC {
        source = obs,
        target = tail obs,
        morph = do
            _ <- tail obs
            fmap Var $ iterate (+ 1) 1
    }

---- Morphisms

-- | Identity morphism.
id :: ObC -> HomC
id (ObC obs) = HomC {
    source = obs,
    target = obs,
    morph = zipWith (\_ i -> Var i) obs (iterate (+ 1) 0)
    }

-- | Composition of morphisms.
(.) :: HomC -> HomC -> HomC
g . f = HomC {
        source = source f,
        target = target g,
        morph = fmap (subst $ morph f) (morph g)
    }

subst :: [Term Int] -> Term Int -> Term Int
subst s (Var i) = s !! i
subst s (Pred p vs) = Pred p (fmap (subst s) vs)

-- Precondition:
--   ft ob == target f
pullback :: HomC -> ObC -> ObC
pullback f (ObC obs) =
    ObC $ (subst (morph f) (head obs)) : (source f)

-- Precondition
--   ft ob == target f
q :: HomC -> ObC -> HomC
q f ob@(ObC obs) = 
    let ObC fstar = pullback f ob
    in HomC {
        source = fstar,
        target = obs,
        morph = (Var 0) : (incr $ morph f)
    }

incr :: [Term Int] -> [Term Int]
incr [] = []
incr (t:ts) = fmap (+ 1) t : incr ts

-----
-- Instances
-----

instance Functor Term where
    fmap f (Var x) = Var (f x)
    fmap f (Pred a xs) = Pred a (map (fmap f) xs)

instance Show ObC where
    show (ObC []) = "[obQ||]"
    show (ObC (o:[])) = "[obQ|" ++ showTerm o ++ "|]"
    show (ObC (o:os)) = "[obQ|" ++ showTermList os ++ ", " ++ showTerm o ++ "|]"

instance Show HomC where
	show f =
	    "[homQ|" ++ showTermList (source f) ++
	    " :- " ++ showJudgList (zip (morph f) (target f)) ++
	    "|]"

showTerm :: Show var => Term var -> String
showTerm (Var v) = show v
showTerm (Pred a vs) = case vs of
    [] -> a
    vs -> a ++ "(" ++ showTermList vs ++ ")"

showTermList :: Show var => [Term var] -> String
showTermList [] = ""
showTermList (v:[]) = showTerm v
showTermList (v:vs) = showTermList vs ++ ", " ++ showTerm v

showJudgList :: Show var => [(Term var,Term var)] -> String
showJudgList [] = ""
showJudgList ((trm,typ):[]) = showTerm trm ++ ":" ++ showTerm typ
showJudgList ((trm,typ):js) = showJudgList js ++ ", " ++ showTerm trm ++ ":" ++ showTerm typ
