
module Rhodium.Context where

import Control.Exception.Base (assert)

type Label = String

data UpTerm var
  = UpVar var
  | UpPred Label [DnTerm var]
  -- ^ variables and atoms
  | UpPi (UpTerm var) (UpTerm var)
  -- ^ dependant product
  | UpSigma (UpTerm var) (UpTerm var)
  -- ^ dependant sum
  | UpWType (UpTerm var) (UpTerm var)
  -- ^ W-types
  | UpType
  -- ^ the type of Types
  deriving (Eq,Show)

data DnTerm var
  = DnVar var
  | DnPred Label [DnTerm var]
  -- ^ variables and atoms
  | DnType (UpTerm var)
  -- ^ type reflected as a term
  | DnLambda (DnTerm var)
  | DnApp (DnTerm var) (DnTerm var)
  -- ^ dependant product
  | DnPair (DnTerm var) (DnTerm var)
  | DnSplit (DnTerm var)
  -- ^ dependant sum
  | DnSup (DnTerm var) (DnTerm var)
  | DnWRec (UpTerm var) (DnTerm var)
  deriving (Eq,Show)

liftTerm :: DnTerm var -> UpTerm var
liftTerm (DnVar n) = UpVar n
liftTerm (DnPred s ts) = UpPred s ts
liftTerm (DnType typ) = typ
-- other terms are not liftable

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

-----
-- Structural rules
-----

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
    -- pre-condition
    assert (target f == source g) $
    -- code
    HomC {
        source = source f,
        target = target g,
        morph = map (substDn $ morph f) (morph g)
    }

---- Dependent projection
ft :: ObC -> ObC
ft (ObC ob) = ObC $ tail ob

-- | Build a canonical projection morphism out of this object.
--   ## TODO: proj has a special case on 'DnApp'
proj :: ObC -> HomC
proj (ObC obs) =
    -- pre-condition
    assert (not $ null obs) $
    -- code
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
    -- pre-condition
    assert (tail obs == target f) $
    -- code
    ObC $ (substUp (morph f) (head obs)) : (source f)

q :: HomC -> ObC -> HomC
q f ob@(ObC obs) =
    assert (tail obs == target f) $
    let ObC fstar = pullback f ob
    in HomC {
        source = fstar,
        target = obs,
        morph = (DnVar 0) : (offset 1 $ morph f)
    }

-- helpers
substUp :: [DnTerm Int] -> UpTerm Int -> UpTerm Int
substUp s (UpVar i) =
    liftTerm $ s !! i
    -- ^ ## WRONG: 'DnApp' counts for two objects
substUp s (UpPred p vs) = UpPred p (map (substUp s) vs)
substUp s (UpPi a b) =


substDn :: [DnTerm Int] -> DnTerm Int -> DnTerm Int
substDn s (DnVar i) =
    s !! i
    -- ## WRONG: 'DnApp' counts for two objects
substDn s (DnPred p vs) =
    DnPred p (map (substDn s) vs)
substDn s (DnLambda f) =
    DnLambda (substDn s f) -- ## WRONG !!!

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
    let upB:upA:gamma = target b
        f:_:bs = morph b
    in HomC {
        source = tail (source b),
        target = (UpPi upA upB) : gamma,
        morph  = (DnLambda f) : bs
    }

-- ∏-ELIM
app :: HomC -> HomC
app g =
    -- pre-condition: a == upA
    let a:(UpPi upA upB):gamma = target g
        x:f:morphs = morph g
    in HomC {
        source = source g,
        target = upB : upA : gamma,
        morph  = (DnApp f x) : morphs
    }

-- k :  [Г] -> [Г,∏(A,B)] , c : [Г] -> [Г,A]
app2 :: HomC -> HomC -> HomC
app2 k a =
    -- pre-conditions
    assert (source k == source a) $
    assert (let (UpPi _ _):g = target k in source k == g) $
    assert (let _:g = target a in source a == g) $
    assert (let (UpPi upA _):_ = target k
                upA':_ = target a
            in upA == upA') $
    -- code
    let (UpPi upA upB):gamma = target k
        f:_ = morph k
        x:morphs = morph a
    in HomC {
        source = gamma,
        target = upB : upA : gamma,
        morph  = (DnApp f x) : morphs
    }

-----
-- W-Types
-----

-- W-FORM
w :: ObC -> ObC
w (ObC (b:a:os)) = ObC $ (UpWType a b) : os

-- W-INTRO
-- for each object [Г,A,B]
-- a map [Г,A,∏(B,p^*_B p^*_A W(A,B))] -> [Г,W(A,B)]
sup :: ObC -> HomC
sup (ObC (b:a:gamma)) =
    let ppw = UpWType a b  -- ## TODO: should be a pullback of p^*_A, P^*_B
    in HomC {
        source = (UpPi b ppw):a:gamma,
        target = (UpWType a b):gamma,
        morph  = (DnSup (DnVar 1) (DnVar 0)):(offset 2 $ morph $ unit (ObC gamma))
    }

-- W-ELIM
-- for each map 'd', a map 'wrec' such that
--   wrec . sup = d . λ(wrec . app(...))
wrec :: HomC -> HomC
wrec d =
    let 
    in HomC {
        source = tail $ target d,
        target = target d,
        morph  = []
    }


-----
-- Validation
-----

-- | Check that the given object is valid in the given environment containing
--   bindings for named values.
-- validOb  :: Env -> ObC -> Bool


-- | Check that the given morphism is valid in the given environment containing
--   bindings for named values.
-- validHom :: Env -> HomC -> Bool


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
