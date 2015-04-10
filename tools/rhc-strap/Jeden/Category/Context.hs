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
Module:         Jeden.Category.Context
Description:    Category of Contexts of Dependent Type Theory
Copyright:      (c) Nicolas Godbout, Polytechnique Montréal
Maintainer:     nicolas.godbout@polymtl.ca

The data type 'CatCtx' represents arrows taking contexts to contexts in
dependent type theory in the style of Martin-Löf.
-}
module Jeden.Category.Context where

-- alt-base modules
import Alt.Abstract.Category

-- Jeden modules
import Data.Naturals

-- base modules
import Data.ByteString (ByteString)
import Data.Ord (Ord(..))
import Data.Eq (Eq(..))
import Data.Int
import Data.List (map)
import Data.Maybe (Maybe(..))

import Prelude (Num(..), undefined, otherwise, error)

-- µPrelude
infixr 0 $
($) :: (a -> b) -> a -> b
{-# INLINE ($) #-}
($) f = f
-- µPrelude


{- | Arrows of a contextual category over a Martin-Löf Dependent Type Theory.
-}
data CatCtx a b where

    ----- Structural -----
    Id          :: ObjCtx n
                -> CatCtx n n

    Proj        :: CatCtx m n -> Type
                -> CatCtx (Succ m) n

    Section     :: CatCtx m n -> Term -> Type
                -> CatCtx m (Succ n)

    ----- Pi-structure ------
    Lambda      :: CatCtx (Succ (Succ n)) (Succ n)
                -> CatCtx n (Succ n)
    -- ## FIX: to make compatible with the base structure

    App         :: CatCtx n (Succ n) -> CatCtx n (Succ n)
                -> CatCtx n (Succ (Succ n))
    -- ## FIX: to make compatible with the base structure

    ----- Sigma-structure -----


-- composition where the condition source g == target f is assumed valid
-- ## convert to a checker with ErrorArrow
dot :: CatCtx b c -> CatCtx a b
    -> CatCtx a c

Id o2 `dot` Id o1 = Id o2

Proj m a `dot` Id o = Proj m a
Proj m2 a2 `dot` Proj m1 a1 = Proj (Proj m2 a2 `dot` m1) a1
Proj m2 a `dot` Section m1 f b = m2 `dot` m1

Section m f b `dot` Id o = Section m f b
Section m2 f b `dot` m1 = Section (m2 `dot` m1) (subst m1 f) b

-- end dot


subst :: CatCtx m n -> Term -> Term
subst m (TmVar i) = m !! i
subst m (Term a ts) = Term a (map (subst m) ts)

{- | Get the Term at index /n/ in the arrow.
-}
(!!) :: CatCtx m n -> Int -> Term
f !! n
    | n < 0         = error "Context.(!!): negative index\n"
    | otherwise     = go f n
        where
            go :: forall m n. CatCtx m n -> Int -> Term
            go (Id o) m
                | grade o >= m  = TmVar n
                | otherwise     = error "Context.(!!): index out-of-rage\n"
            go (Proj f _) m
                = go f m
            go (Section f trm _) m
                | m == 0        = trm
                | otherwise     = go f (m - 1)

{- | Objects of the category of Dependent Type Theory, /i.e./ contexts.
-}
data ObjCtx a where
    Init        :: ObjCtx Zero
    Obj         :: ObjCtx n -> Type -> ObjCtx (Succ n)

deriving instance Eq (ObjCtx a)

hd :: ObjCtx (Succ n) -> Type
hd (Obj _ t) = t

ft :: ObjCtx (Succ n) -> ObjCtx n
ft (Obj o _) = o

grade :: ObjCtx n -> Int
grade = go 0
    where
        go :: forall n. Int -> ObjCtx n -> Int
        go n Init = n
        go n (Obj o _) = go (n + 1) o


-----
-- Structural morphisms
-----

proj :: ObjCtx (Succ n)
     -> CatCtx (Succ n) n
proj (Obj o t) = Proj (Id o) t


{- | Pullback in the Contextual Category, corresponding to substitution.
The given morphism describes a substitution for all types of the given object
except the last one.
-}
pullback :: CatCtx m n -> ObjCtx (Succ n)
         -> CatCtx (Succ m) (Succ n)
pullback f (Obj o t) =
    Section (fix $ Proj f t) (TmVar 0) t
        where
            -- push Proj under Section to recover a canonical form
            fix :: forall m n. CatCtx m n -> CatCtx m n
            fix (Proj (Section m f b) t) = Section (fix $ Proj m t) f b
            fix m = m

-- equality witness may be an evaluation map, even a unifier
data EqCtx a b where
    EqualCtx    :: ObjCtx n -> EqCtx n n



data Term where
    TmVar       :: Int -> Term
    Term        :: ByteString -> [Term] -> Term
    deriving (Eq)

data Type where
    TyVar       :: Int -> Type
    Type        :: ByteString -> [Type] -> Type
    deriving (Eq)


instance Category CatCtx where
    type EqC CatCtx a b = EqCtx a b
    type Obj CatCtx a   = ObjCtx a

    source (Id obj)         = obj
    source (Proj m t)       = Obj (source m) t
    source (Section m _ _)  = source m

    target (Id obj)         = obj
    target (Proj m _)       = target m
    target (Section m _ t)  = Obj (target m) t

    idC = Id

    dotW (EqualCtx o) (Id _) (Id _) = Id o

    dotW (EqualCtx o) (Proj m a) (Id _) = Proj m a
    dotW (EqualCtx o) (Proj m2 a2) (Proj m1 a1) = Proj (Proj m2 a2 `dot` m1) a1
    dotW (EqualCtx o) (Proj m2 a) (Section m1 f b) = m2 `dot` m1

    dotW (EqualCtx o) (Section m f b) (Id _) = Section m f b
    dotW (EqualCtx o) (Section m2 f b) m1 = Section (m2 `dot` m1) (subst m1 f) b
