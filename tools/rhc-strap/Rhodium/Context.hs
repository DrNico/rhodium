{-# LANGUAGE
    ScopedTypeVariables
  #-}

module Rhodium.Context where

import Control.Monad (zipWithM)
import Control.Monad.Error.Class
import Test.QuickCheck
import Test.QuickCheck.Monadic (PropertyM, run, monadic)

import Prelude hiding ((.), id)

data Term var
  = Var var
  | Pred String [Term var]
  deriving (Eq,Show)

type ObC  = [Term Int]
data HomC = HomC {
        source      :: ObC,
        target      :: ObC,
        morph       :: [Term Int]
    }
    deriving (Eq,Show)

-- ## TODO: introduce a 'Checked' Monad, tagging terms that have been type-checked.
-- pure  :: ObC -> Checked ObC
-- id    :: Checked ObC -> Checked HomC
-- <^.^> :: HomC -> HomC -> Checked HomC
-- <.^>  :: Checked HomC -> HomC -> Checked HomC
-- <^.>  :: HomC -> Checked HomC -> Checked HomC
-- <.>   :: Checked HomC -> Checked HomC -> Checked HomC


---- Dependent projection
ft :: ObC -> ObC
ft = tail

-- Pre-condition Ob_n , n > 0
proj :: ObC -> HomC
proj obs = HomC {
        source = obs,
        target = ft obs,
        morph = do
            _ <- ft obs
            fmap Var $ iterate (+ 1) 1
    }

---- Morphisms

-- | Identity morphism.
id :: ObC -> HomC
id obs = HomC {
    source = obs,
    target = obs,
    morph = zipWith (\_ i -> Var i) obs (iterate (+ 1) 0)
    }

-- | Composition of morphisms.
(.) :: HomC -> HomC -> HomC
g . f = HomC {
        source = source f,
        target = target g,
        morph = fmap (subst $ morph f) (morph g)
    }

-- | Composition of morphisms in a Contextual Category.
(<.>) :: (Error e, MonadError e m)
    => HomC -> HomC
    -> m HomC
g <.> f =
    if target f == source g
    then return HomC {
            source = source f,
            target = target g,
            morph = fmap (subst $ morph g) (morph f)
        }
    else throwError $ strMsg "error"

subst :: [Term Int] -> Term Int -> Term Int
subst s (Var i) = s !! i
subst s (Pred p vs) = Pred p (fmap (subst s) vs)

-- Precondition:
--   ft ob == target f
pullback :: HomC -> ObC -> ObC
pullback f ob =
    (subst (morph f) (head ob)) : (source f)

-- Precondition
--   ft ob == target f
q :: HomC -> ObC -> HomC
q f ob = 
    let fstar = pullback f ob 
    in HomC {
        source = fstar,
        target = ob,
        morph = (Var 0) : (incr $ morph f)
    }

incr :: [Term Int] -> [Term Int]
incr [] = []
incr (t:ts) = fmap (+ 1) t : incr ts

-----
-- Instances
-----

instance Functor Term where
    fmap f (Var x) = Var (f x)
    fmap f (Pred a xs) = Pred a (map (fmap f) xs)


