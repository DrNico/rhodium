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


-- | Instances should satisfy the following equations:
-- prop> grade terminal == 0
-- prop> grade (ft x) == grade x - 1
--     * pullback is functorial
class Contextual typ where
    type Cat typ    :: * -> * -> *

    grade           :: typ n -> Int
    terminal        :: typ Zero
    ft              :: typ (Succ n) -> typ n
    proj            :: typ (Succ n) -> Cat typ (Succ n) n
    pullback        :: typ (Succ n) -> Cat typ n m
                    -> Cat typ (Succ n) (Succ m)

-- | Instances should satisfy the following equations:
-- prop> app . section a . section (lambda (section b)) == section b . section a
class Contextual typ => PiStructure typ where
    -- | Transform a section (a,b) : [A, B] into a section (lambda b) : [Pi(A,B)]
    lambda          :: Cat typ n (Succ (Succ m))
                    -> Cat typ n (Succ m)

    -- | Transform a section (k,a) : [Pi(A,B),A] into a section (a,b) : [A,B]
    app             :: Cat typ n (Succ (Succ m))
                    -> Cat typ n (Succ (Succ m))
