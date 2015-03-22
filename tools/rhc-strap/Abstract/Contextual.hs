{-# LANGUAGE
    TypeFamilies
  #-}

{-|
Module          : Abstract.Contextual
Description     : Class of Contextual Categories
Copyright       : (c) Nicolas Godbout, 2015
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : Experimental
-}
module Abstract.Contextual where

import Abstract.Category (Category(..))
import Data.Naturals (Zero, Succ)

import Prelude (Int)


-- | An instance should satisfy the following equations:
--     * @grade terminal == 0@
--     * @grade (ft x) == grade x - 1@
--     * pullback is functorial
class Contextual typ where
    type Cat typ    :: * -> * -> *

    grade           :: typ n -> Int
    terminal        :: typ Zero
    ft              :: typ (Succ n) -> typ n
    proj            :: typ (Succ n) -> Cat typ (Succ n) n
    pullback        :: typ (Succ n) -> Cat typ n m
                    -> Cat typ (Succ n) (Succ m)

{-
class Contextual cat => PiStructure cat where
    type Pi cat a b :: *

    lambda          :: Hom cat g (g a b)
                    -> Hom cat g (g, Pi cat a b)
    app             :: Hom 
-}
