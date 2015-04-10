{-# LANGUAGE
        NoImplicitPrelude,
        GADTs,
        RankNTypes,
        TypeFamilies,
        TypeOperators,
        OverloadedStrings,
        StandaloneDeriving,
        ScopedTypeVariables
    #-}

{- |
Module:         Jeden.Category.DependentTypeTheory
Description:    Contextual category of judgments of dependent type theory
Copyright:      (c) Nicolas Godbout, Polytechnique Montréal
Maintainer:     nicolas.godbout@polymtl.ca

The data type 'CatDTT' represents arrows taking contexts to contexts in
dependent type theory in the style of Martin-Löf.
-}
module Jeden.Category.DependentTypeTheory where

-- alt-base modules
import Alt.Abstract.Category

-- Jeden modules
import Data.Naturals

-- base modules
import Data.ByteString (ByteString)
import Data.Eq (Eq(..))
import Data.Int
import Data.List (map)
import Data.Maybe (Maybe(..))

import Prelude (Num(..), undefined, otherwise, error)

{- | Arrows of a contextual category over a Martin-Löf Dependent Type Theory.
-}
data CatDTT a b where

    ----- Structural -----
    Id          :: ObjDTT n
                -> CatDTT n n

    Proj        :: CatDTT m n -> Type
                -> CatDTT (Succ m) n

    Section     :: CatDTT m n -> Term -> Type
                -> CatDTT m (Succ n)

    ----- Pi-structure ------
    Lambda      :: CatDTT (Succ (Succ n)) (Succ n)
                -> CatDTT n (Succ n)

    App         :: CatDTT n (Succ n) -> CatDTT n (Succ n)
                -> CatDTT n (Succ (Succ n))

    ----- Sigma-structure -----


-- composition where the condition source g == target f is assumed valid
-- ## convert to a checker with ErrorArrow
dot :: CatDTT b c -> CatDTT a b
    -> CatDTT a c

Id o2 `dot` Id o1 = Id o2

Proj m a `dot` Id o = Proj m a
Proj m2 a2 `dot` Proj m1 a1 = Proj (Proj m2 a2 `dot` m1) a1
Proj m2 a `dot` Section m1 f b = m2 `dot` m1

Section m f b `dot` Id o = Section m f b
Section m2 f b `dot` m1 = Section (m2 `dot` m1) (subst m1 f) b


subst :: CatDTT m n -> Term -> Term
subst m (TmVar i) = m !! i
subst m (Term a ts) = Term a (map (subst m) ts)

{- | Get the Term at index /n/ in the arrow.
-}
(!!) :: CatDTT m n -> Int -> Term
Id _ !! n = TmVar n  -- ## BUG: need to count offsets going down other terms
Proj f _ !! n = f !! n
Section _ trm _ !! 0 = trm
Section f _ _ !! n = f !! (n - 1)


{- | Objects of the category of Dependent Type Theory, /i.e./ contexts.
-}
data ObjDTT a where
    Init        :: ObjDTT Zero
    Obj         :: ObjDTT n -> Type -> ObjDTT (Succ n)

deriving instance Eq (ObjDTT a)

hd :: ObjDTT (Succ n) -> Type
hd (Obj _ t) = t

ft :: ObjDTT (Succ n) -> ObjDTT n
ft (Obj o _) = o

-----
-- Structural morphisms
-----

proj :: ObjDTT (Succ n)
     -> CatDTT (Succ n) n
proj (Obj o t) = Proj (Id o) t


pullback :: CatDTT m n -> ObjDTT (Succ n)
         -> CatDTT (Succ m) (Succ n)
pullback f (Obj o t) =
    Section (Proj f t) (TmVar 0) t


-- equality witness may be an evaluation map, even a unifier
data EqDTT a b where
    EqualDTT    :: ObjDTT n -> EqDTT n n



data Term where
    TmVar       :: Int -> Term
    Term        :: ByteString -> [Term] -> Term
    deriving (Eq)

data Type where
    TyVar       :: Int -> Type
    Type        :: ByteString -> [Type] -> Type
    deriving (Eq)


instance Category CatDTT where
    type EqC CatDTT a b = EqDTT a b
    type Obj CatDTT a   = ObjDTT a

    source (Id obj)         = obj
    source (Proj m t)       = Obj (source m) t
    source (Section m _ _)  = source m

    target (Id obj)         = obj
    target (Proj m _)       = target m
    target (Section m _ t)  = Obj (target m) t

    idC = Id

    dotW (EqualDTT o) (Id _) (Id _) = Id o

    dotW (EqualDTT o) (Proj m a) (Id _) = Proj m a
    dotW (EqualDTT o) (Proj m2 a2) (Proj m1 a1) = Proj (Proj m2 a2 `dot` m1) a1
    dotW (EqualDTT o) (Proj m2 a) (Section m1 f b) = m2 `dot` m1

    dotW (EqualDTT o) (Section m f b) (Id _) = Section m f b
    dotW (EqualDTT o) (Section m2 f b) m1 = Section (m2 `dot` m1) (subst m1 f) b
