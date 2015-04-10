{-# LANGUAGE
    GADTs,
    TypeFamilies,
    TypeOperators
  #-}

{-|
Module          : Data.Naturals
Description     : Type-level and Data-level Peano Naturals
Copyright       : (c) 2015 Nicolas Godbout
				  (c) 2014 Kenneth Foner
License         : BSD3
Maintainer      : nicolas.godbout@gmail.com
Stability       : Experimental

Credits to Kenneth Foner for code taken from the PeanoWitnesses package, released
under a BSD3-style licence.
-}
module Data.Naturals where

infixr 3 :+:
infixr 3 :-:

data Zero
data Succ n

data Natural n where
    Zero        :: Natural Zero
    Succ        :: Natural n -> Natural (Succ n)

plus :: Natural m -> Natural n -> Natural (m :+: n)
plus x Zero = x
plus x (Succ y) = Succ (plus x y)

class ReifyNatural n where
    reifyNatural :: Natural n

instance ReifyNatural Zero where
    reifyNatural = Zero

instance ReifyNatural n => ReifyNatural (Succ n) where
    reifyNatural = Succ reifyNatural

type family x :+: y

type instance x :+: Zero = x
type instance x :+: Succ y = Succ (x :+: y)

type family x :-: y

type instance x :-: Zero = x
type instance Succ x :-: Succ y = x :-: y
