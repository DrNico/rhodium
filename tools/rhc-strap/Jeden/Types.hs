{-# LANGUAGE
    GADTs, TypeFamilies
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

import Prelude (Eq(..), Int, undefined)


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
            terms = undefined,
            types = ob
        }

    g . f =
        Map {
            context = context f,
            terms = undefined,
            types = types g
        }

instance Contextual Obj where
    type Cat Obj = Map

    grade = length . obj

    terminal = Obj Nil

    ft (Obj o) = Obj (init o)

    proj t = (id (ft t)) { context = obj t }

    pullback = undefined

