{-# LANGUAGE
	BangPatterns, TypeFamilies
  #-}

module Rhodium.Contextual where

import Control.Exception.Base (assert)
import Data.Eq (Eq(..))
import Data.Functor (Functor(..))
import Data.Int (Int)
import Data.String (String)

import Prelude (($), ($!), (+), (-), error)

type Label = String

data Types var
  = TyNil
  | TyCons (Type var) (Types var)
  deriving (Eq)
  
data Type var
  = TyVar var
  | TyAtom Label (Terms var)
  | TyUniverse
  --
  | TyPi (Type var) (Type var)
  -- ^ product types
  deriving (Eq)

data Terms var
  = TmNil
  | TmCons (Term var) (Terms var)
  deriving (Eq)

data Term var
  = TmVar var
  | TmAtom Label (Terms var)
  | TmType (Type var)
  --
  | TmLambda (Term var)
  | TmApp (Term var) (Term var)
  -- ^ product types
  deriving (Eq)

data ObC = ObC (Types Int) deriving (Eq)

data HomC = HomC {
		source :: Types Int,
		target :: Types Int,
		morph  :: Terms Int
	}
	deriving (Eq)

infixr 9 <.>

(<.>) :: HomC -> HomC -> HomC
g <.> f = comp $ Hom2C g f


-----
-- Structural rules
-----


-- | Identity morphism.
unit :: ObC -> HomC
unit (ObC obs) = HomC {
	    source = obs,
	    target = obs,
	    morph  = mkId 0 obs
    }
	where
	mkId :: Int -> Types Int -> Terms Int
	mkId _ TyNil = TmNil
	mkId n (TyCons _ typs) = TmCons (TmVar n) (mkId (n + 1) typs)


-- | Composition of morphisms.
comp :: Hom2C -> HomC
comp (Hom2C g f) =
    -- pre-condition
    assert (target f == source g) $
    -- code
    HomC {
        source = source f,
        target = target g,
        morph  = map (substTerm $ morph f) (morph g)
    }


data Hom2C = Hom2C {
        lpart       :: HomC,
        rpart       :: HomC
    }
    deriving (Eq)


pullback :: HomC -> ObC -> ObC
pullback f (ObC obs) =
    -- pre-condition
    assert (tail obs == target f) $
    -- code
    ObC $ TyCons (substType (morph f) (head obs)) (source f)


qullback :: HomC -> ObC -> HomC
qullback f ob@(ObC obs) =
    let ObC fstar = pullback f ob
    in HomC {
        source = fstar,
        target = obs,
        morph = TmCons (TmVar 0) (offset 1 $ morph f)
    }


-----
-- Helper functions
-----

substType :: Terms Int -> Type Int -> Type Int
substType trms (TyVar n) =
	case trms !! n of
		TmType typ -> typ
substType trms (TyAtom a as) =
	TyAtom a (map (substTerm trms) as)
substType trms TyUniverse =
	TyUniverse
substType trms (TyPi a b) =
	TyPi (substType trms a) (substType trms b)
-- ^ not a total function, some terms are not liftable into types


substTerm :: Terms Int -> Term Int -> Term Int
substTerm trms (TmVar n) =
	trms !! n
substTerm trms (TmAtom a as) =
	TmAtom a (map (substTerm trms) as)
substTerm trms (TmLambda f) =
	TmLambda (substTerm trms f)
substTerm trms (TmApp f x) =
	TmApp (substTerm (TmCons x trms) f) (substTerm trms x)


offset :: Int -> Terms Int -> Terms Int
offset n = map (fmap $ \m -> let !r = n + m in r)

-----
-- Instances
-----

class Scope t where
	type Members t :: * -> *

	-- List-like
	(!!)   :: t a -> Int -> Members t a
	length :: t a -> Int
	head   :: t a -> Members t a
	tail   :: t a -> t a

	-- Functor-like
	map    :: (Members t a -> Members t b) -> t a -> t b

-----

instance Scope Types where
	type Members Types = Type

	TyNil !! idx = error "index too large"
	TyCons typ typs !! 0 = typ
	TyCons typ typs !! n = typs !! (n - 1)

	length = foldlS (\n _ -> n + 1) 0
		where
		foldlS :: (a -> Type b -> a) -> a -> Types b -> a
		foldlS f x TyNil = x
		foldlS f x (TyCons typ typs) =
			let !x' = f x typ in foldlS f x' typs

	head TyNil = error "empty Types"
	head (TyCons trm _) = trm

	tail TyNil = error "empty Types"
	tail (TyCons _ trms) = trms

	map f TyNil = TyNil
	map f (TyCons typ typs) = TyCons (f typ) (map f typs)

instance Scope Terms where
	type Members Terms = Term

	TmNil !! idx = error "Index out of range"
	TmCons (TmApp f _) trms !! 0 = f
	TmCons (TmApp _ x) trms !! 1 = x
	TmCons (TmApp _ _) trms !! n = trms !! (n - 2)
 	TmCons trm trms !! 0 = trm
	TmCons trm trms !! n = trms !! (n - 1)

	length = foldlS (\n _ -> n + 1) 0
		where
		foldlS :: (a -> Term b -> a) -> a -> Terms b -> a
		foldlS f x TmNil = x
		foldlS f x (TmCons trm trms) =
			let !x' = f x trm in foldlS f x' trms

	head TmNil = error "empty Terms"
	head (TmCons (TmApp k _) _) = k
	head (TmCons trm _) = trm

	tail TmNil = error "empty Terms"
	tail (TmCons (TmApp _ x) trms) = TmCons x trms
	tail (TmCons _ trms) = trms

	map f TmNil = TmNil
	map f (TmCons (TmApp k x) trms) = TmCons (TmApp (f k) (f x)) (map f trms)
	map f (TmCons trm trms) = TmCons (f trm) (map f trms)

instance Functor Type where
	fmap f (TyVar v) = TyVar (f v)
	fmap f (TyAtom a as) = TyAtom a (map (fmap f) as)
	fmap f (TyUniverse) = TyUniverse
	fmap f (TyPi a b) = TyPi (fmap f a) (fmap f b)

instance Functor Term where
	fmap f (TmVar v) = TmVar (f v)
	fmap f (TmAtom a as) = TmAtom a (map (fmap f) as)
	fmap f (TmType v) = TmType (fmap f v)
	fmap f (TmLambda k) = TmLambda (fmap f k)
	fmap f (TmApp k x) = TmApp (fmap f k) (fmap f x)
