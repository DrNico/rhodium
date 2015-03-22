{-# LANGUAGE
    GADTs, TypeFamilies, RankNTypes
  #-}

{-|
Module          : Jeden.Types
Description     : Haskell data-types for the Jeden type system
Copyright       : (c) Nicolas Godbout, 2015
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : Experimental
-}
module Jeden.Types where

import Abstract.Category (Category(..))
import Abstract.Contextual (Contextual(..))
import Data.ListN

import Data.Functor (Functor(..))

import Prelude (Eq(..), Int, (+))


data Type var where
    TyVar           :: var -> Type var
    deriving (Eq)
    
data Term var where
    TmVar           :: var -> Term var
    deriving (Eq)

newtype Obj n = Obj {
    obj     :: ListN n (Type Int)
}


data Map n m = Map {
    context :: ListN n (Type Int),
    terms   :: ListN m (Term Int),
    types   :: ListN m (Type Int)
} deriving (Eq)

instance Category Map where
    type Ob Map = Obj
    
    source = Obj . context
    target = Obj . types

    id (Obj ob) = Map {
            context = ob,
            terms = go 0 ob,
            types = ob
        }
        where
            go :: forall n. Int -> ListN n (Type Int) -> ListN n (Term Int)
            go _ Nil = Nil
            go n (Cons _ r) = Cons (TmVar n) (go (n + 1) r)

    g . f =
        Map {
            context = context f,
            terms = fmap (substTerm (terms f)) (terms g),
            types = fmap (substType (terms f)) (types g)
        }

instance Contextual Obj where
    type Cat Obj = Map

    grade = length . obj

    terminal = Obj Nil

    ft (Obj o) = Obj (init o)

    proj t = (id (ft t)) { context = obj t }

    pullback (Obj o) f =
        Map {
            context = snoc (context f) (head o),
            terms = snoc (terms f) (TmVar (length o)),
            types = snoc (types f) (head o)
        }


-----
-- Helper functions
-----

substTerm :: ListN n (Term Int) -> Term Int -> Term Int
substTerm ts (TmVar n) = ts !! n

substType :: ListN n (Term Int) -> Type Int -> Type Int
substType ts (TyVar n) = liftTerm (ts !! n)

liftTerm :: Term Int -> Type Int
liftTerm (TmVar n) = TyVar n

