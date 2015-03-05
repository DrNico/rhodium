
module Rhodium.Context where

import Control.Exception.Base (assert)
import Control.Monad.Error.Class

data UpTerm var
  = UpVar var
  | UpPred String [UpTerm var]
  | UpPi (UpTerm var) (UpTerm var)
  | UpSigma (UpTerm var) (UpTerm var)
  deriving (Eq,Show)

data DnTerm var
  = DnVar var
  | DnPred String [DnTerm var]
  | DnLambda (DnTerm var)
  | DnPair (DnTerm var) (DnTerm var)
  deriving (Eq,Show)

liftTerm :: DnTerm var -> UpTerm var
liftTerm (DnVar v)     = UpVar v
liftTerm (DnPred s vs) = UpPred s (map liftTerm vs)
-- ## TODO: figure out if formers are liftable


-- | An Object of the Contextual Category: a context in type theory.
--   A context is a list of Terms of type Type, with variables
--   represented by deBruijn indices indicating an *offset* in the list, relative
--   to the current term.
data ObC  = ObC [UpTerm Int] deriving (Eq)

-- | A Morphism of the Contextual Category: a term in type theory.
data HomC = HomC {
        source      :: [UpTerm Int],
        target      :: [UpTerm Int],
        morph       :: [DnTerm Int]
    }
    deriving (Eq)

-- | A Pair of *composable* morphisms of the Contextual Category.
--   Such a constructed pair should always respect the condition
--      source lpart == target rpart
data Hom2C = Hom2C {
        lpart       :: HomC,
        rpart       :: HomC
    }
    deriving (Eq)

(<.>) :: HomC -> HomC -> HomC
g <.> f = comp $ Hom2C g f

infixr 9 <.>

---- Morphisms

-- | Identity morphism.
unit :: ObC -> HomC
unit (ObC obs) = HomC {
    source = obs,
    target = obs,
    morph = zipWith (\_ i -> DnVar i) obs (iterate (+ 1) 0)
    }

-- | Composition of morphisms.
comp :: Hom2C -> HomC
comp (Hom2C g f) =
    assert (target f == source g) $
    HomC {
        source = source f,
        target = target g,
        morph = map (substDn $ morph f) (morph g)
    }

substDn :: [DnTerm Int] -> DnTerm Int -> DnTerm Int
substDn s (DnVar i) = s !! i
substDn s (DnPred p vs) =
    DnPred p (map (substDn s) vs)
substDn s (DnLambda f) =
    DnLambda (substDn s f)

---- Dependent projection
ft :: ObC -> ObC
ft (ObC ob) = ObC $ tail ob

-- | Build a canonical projection morphism out of this object.
proj :: ObC -> HomC
proj (ObC obs) =
    assert (not $ null obs) $
    HomC {
        source = obs,
        target = tail obs,
        morph = zipWith (\_ i -> DnVar i) (tail obs) (iterate (+ 1) 1)
    }

-- | True if the given morphism is a section of the canonical projection.
isSection :: HomC -> Bool
isSection f =
    source f == tail (target f) &&
    tail (morph f) == morph (unit (ObC (source f)))

-- | Pullback the canonical projection 'proj' from object 'x' along 'f'
pullback :: HomC -> ObC -> ObC
pullback f (ObC obs) =
    assert (tail obs == target f) $
    ObC $ (substUp (morph f) (head obs)) : (source f)


substUp :: [DnTerm Int] -> UpTerm Int -> UpTerm Int
substUp s (UpVar i) = liftTerm $ s !! i
substUp s (UpPred p vs) = UpPred p (map (substUp s) vs)

q :: HomC -> ObC -> HomC
q f ob@(ObC obs) =
    assert (tail obs == target f) $
    let ObC fstar = pullback f ob
    in HomC {
        source = fstar,
        target = obs,
        morph = (DnVar 0) : (incr $ morph f)
    }

incr :: [DnTerm Int] -> [DnTerm Int]
incr = offset 1

offset :: Int -> [DnTerm Int] -> [DnTerm Int]
offset n = map $ fmap (+ n)
{-# INLINE[2] offset #-}
{-# RULES
    "offset/offset" forall n m ts. offset n (offset m ts) = offset (n + m) ts
  #-}

-----
-- Dependent Functions
-----

-- ∏-FORM
pi :: ObC -> ObC
pi (ObC (b:a:os)) = ObC $ (UpPi a b) : os

-- ∏-INTRO
-- b : [G,A] -> [G,A,B]
-- lambda b : [G] -> [G,Pi(A,B)]
lambda :: HomC -> HomC
lambda b =
    -- pre-conditions
    assert (not (null $ source b)) $
    assert (source b == tail (target b)) $
    -- code
    let upB:upA:gamma = target b
        f:bs = morph b
        -- ## TODO: 'eager' substitution if x has a constant head?
        --   seems like a bad idea...
    in HomC {
        source = gamma,
        target = (UpPi upA upB) : gamma,  -- pi (target k)
        morph  = (DnLambda f) : bs
    }

-- ∏-ELIM
-- k :  [Г] -> [Г,A,B] , c : [Г] -> [Г,A]
app :: HomC -> HomC -> HomC
app k a =
    -- pre-conditions
    assert (source k == source a) $
    assert (let (UpPi _ _):g = target k in source k == g) $
    assert (let _:g = target a in source a == g) $
    assert (let (UpPi upA _):_ = target k
                upA':_ = target a
            in upA == upA') $
    -- code
    let (UpPi upA upB):gamma = target k
        (DnLambda f):_ = morph k
        
        -- ## TODO: shouldn't we unify x and x' instead?
        f' = substDn (morph a) f
    in HomC {
        source = gamma,
        target = upB : upA : gamma,
        morph  = f' : (morph a)
    }

-----
-- Judgmental Equality
-----

class EqJ a where
    eqJ :: ObC -> a -> a -> Bool

instance EqJ (DnTerm a) where
    eqJ gamma (DnPred s1 ts1) (DnPred s2 ts2) =
        s1 == s2 &&
        and (zipWith (eqJ gamma) ts1 ts2)
    -- ## TODO: complete the inference rules

-----
-- Instances
-----

instance Functor DnTerm where
    fmap f (DnVar x)      = DnVar (f x)
    fmap f (DnPred a xs)  = DnPred a (map (fmap f) xs)
    fmap f (DnLambda k)   = DnLambda (fmap f k)
    fmap f (DnPair a b)   = DnPair (fmap f a) (fmap f b)

instance Show ObC where
    show (ObC []) = "[obQ||]"
    show (ObC (o:[])) = "[obQ|" ++ showUpTerm o ++ "|]"
    show (ObC (o:os)) = "[obQ|" ++ showListWith showUpTerm os ++ ", " ++ showUpTerm o ++ "|]"

instance Show HomC where
	show f =
	    "[homQ|" ++ showListWith showUpTerm (source f) ++
	    " :- " ++ showJudgList (zip (morph f) (target f)) ++
	    "|]"

showUpTerm :: Show var => UpTerm var -> String
showUpTerm (UpVar v) = '$' : (show v)
showUpTerm (UpPred a vs) = case vs of
    [] -> a
    vs -> a ++ "(" ++ showListWith showUpTerm vs ++ ")"
showUpTerm (UpPi a b) =
    "∏[" ++ showUpTerm a ++ "]" ++ showUpTerm b
showUpTerm (UpSigma a b) =
    "∑[" ++ showUpTerm a ++ "]" ++ showUpTerm b

showDnTerm :: Show var => DnTerm var -> String
showDnTerm (DnVar v) = '$' : (show v)
showDnTerm (DnPred a vs) = case vs of
    [] -> a
    vs -> a ++ "(" ++ showListWith showDnTerm vs ++ ")"
showDnTerm (DnLambda k) =
    "λ." ++ showDnTerm k
showDnTerm (DnPair a b) =
    "(" ++ showDnTerm a ++ "," ++ showDnTerm b ++ ")"

showListWith :: (v -> String) -> [v] -> String
showListWith s [] = ""
showListWith s (v:[]) = s v
showListWith s (v:vs) = showListWith s vs ++ ", " ++ s v

showJudgList :: (Show var) => [(DnTerm var,UpTerm var)] -> String
showJudgList [] = ""
showJudgList ((trm,typ):[]) = showDnTerm trm ++ ":" ++ showUpTerm typ
showJudgList ((trm,typ):js) = showJudgList js ++ ", " ++ showDnTerm trm ++ ":" ++ showUpTerm typ
