{-# LANGUAGE
    MultiParamTypeClasses, FlexibleInstances
  #-}

{-|
Module          : Rhodium.Builder
Description     : Builder of correct Morphisms in Contextual Categories
Copyright       : (c) Nicolas Godbout, 2015
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : Experimental

The Builder module provides facilities to incrementally build valid morphisms
of a Contextual Category. The Builder monad supports error detection, formation
and accumulation. A morphism, i.e. a typed term, built through the Builder is
necessarily well-formed. Importers of this module should treat morphisms as
opaque objects to ensure validity.
-}
module Rhodium.Builder where

import Control.Applicative
import Control.Monad (Functor(..), Monad(..), unless)
import Control.Monad.Error (Error(..), MonadError(..))
import Data.Eq (Eq(..))
import Data.Int (Int)
import Data.String (String)

import Rhodium.Contextual

import Prelude (($))

type Builder = BuilderM String
newtype BuilderM e a = Builder {
    runBuilder :: a
}

data Judgement var
  = JType
  | JEqT (Type var) (Type var)

instance Functor (BuilderM e)
instance Applicative (BuilderM e)
instance Monad (BuilderM e)
instance Error e => MonadError e (BuilderM e)


check :: Judgement var -> Builder ()
check _ =
    return ()

mkTerm :: Type Label -> Term Label -> Type Label
       -> Builder (HomC)
mkTerm _ _ _ =
    return $ unit terminal

terminal :: ObC
terminal = ObC TyNil

mkPi :: ObC -> Builder (ObC)
mkPi (ObC (TyCons a (TyCons b typs))) =
    return $ ObC $ TyCons (TyPi a b) typs
mkPi (ObC (TyCons _ _)) =
    throwError $ strMsg "Pi-former expected two arguments, received one"
mkPi (ObC TyNil) =
    throwError $ strMsg "Pi-former expected two arguments, received none"

mkLambda :: HomC -> Builder (HomC)
mkLambda b = do
    unless
        (isSection b)
        (throwError $ strMsg "argument of 'lambda' is not a section")
    let TyCons upB (TyCons upA gamma) = target b
    let TmCons f (TmCons x bs) = morph b
    return HomC {
        source = tail (source b),
        target = (TyPi upA upB) `TyCons` gamma, -- ## TODO: make 'upA' capture argument 'x'?
        morph  = (TmLambda f) `TmCons` bs
    }

mkApp :: HomC -> HomC -> Builder HomC
mkApp k x = do
    unless
        (isSection k)
        (throwError $ strMsg "APP rule: 'f' is not a section")
    unless
        (isSection x)
        (throwError $ strMsg "APP rule: 'a' is not a section")
    unless
        (source k == source x)
        (throwError $ strMsg "APP rule: 'f' and 'a' contexts do not coincide")

    let ax `TyCons` _ = target x
    let (TyPi ak bk) `TyCons` gamma = target k
    check (ax `JEqT` ak) -- ## unify?

    let f `TmCons` _ = morph k
    let xx `TmCons` morphs = morph x
    return HomC {
        source = gamma,
        target = bk `TyCons` (ak `TyCons` gamma),
        morph  = (TmApp f xx) `TmCons` morphs
    }
