{-# LANGUAGE
    GADTs
  #-}

{-|
Module          : Data.Naturals
Description     : Type-level and Data-level Peano Naturals
Copyright       : (c) Nicolas Godbout, 2015
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : Experimental
-}
module Data.Naturals where


data Zero
data Succ n

data Natural n where
    Zero        :: Natural Zero
    Succ        :: Natural n -> Natural (Succ n)

class ReifyNatural n where
    reifyNatural :: Natural n

instance ReifyNatural Zero where
    reifyNatural = Zero

instance ReifyNatural n => ReifyNatural (Succ n) where
    reifyNatural = Succ reifyNatural
