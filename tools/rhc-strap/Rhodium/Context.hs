
module Rhodium.Context where

import Control.Monad (zipWithM)
import Control.Monad.Error.Class
import Test.QuickCheck

import Prelude hiding ((.), id)

data Term var
  = Var var
  | Pred String [Term var]
  deriving (Eq,Show)

type ObC  = [Term Int] -- index refers to term of type (ObC !! i)
data HomC = HomC {
        source      :: ObC,
        target      :: ObC,
        morph       :: [Term Int] -- index refers to type in 'source'
    }
    deriving (Eq,Show)

---- Dependent projection
ft :: ObC -> ObC
ft = tail

-- X : Ob :- proj : X -> ft X
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

-- | Composition of morphisms in a Contextual Category.
-- ## TODO: introduce a 'Checked' Monad, tagging terms that have been type-checked.
-- pure  :: ObC -> Checked ObC
-- id    :: Checked ObC -> Checked HomC
-- <^.^> :: HomC -> HomC -> Checked HomC
-- <.^>  :: Checked HomC -> HomC -> Checked HomC
-- <^.>  :: HomC -> Checked HomC -> Checked HomC
-- <.>   :: Checked HomC -> Checked HomC -> Checked HomC
(.) :: (Error e, MonadError e m)
    => HomC -> HomC
    -> m HomC
g . f =
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

pullback :: HomC -> ObC -> ObC
pullback f ob =
    (subst (morph f) (head ob)) : (source f)

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

instance Functor Term where
    fmap f (Var x) = Var (f x)
    fmap f (Pred a xs) = Pred a (map (fmap f) xs)

---- Pullback
-- Pullback respects the equations
--    proj (target $ q f ob) . q f ob == f . proj (pullback f ob)
--    pullback (id (ft ob)) ob == ob
--    q (id (ft ob)) ob == id ob
--    pullback (g . f) ob == pullback g (pullback f ob)
--    q (g . f) ob == q g (pullback f ob) . q f ob
-- with 'on-the-nose' equality.

-----
-- QuickCheck
-----

newtype MkOb = MkOb {
        unMkOb :: [Term Int]
    }
    deriving (Show)

instance Arbitrary MkOb where
    arbitrary = sized $ \siz ->
        case siz of
            0 -> return $ MkOb []
            1 -> do
                atom <- resize 2 arbitrary
                return $ MkOb [Pred atom []]
            _ -> do
                ob <- oneof [ do
                        i <- choose (0,siz - 2)
                        return $ Var i
                    , do
                        atom <- resize 2 arbitrary
                        return $ Pred atom []
                    ]
                MkOb obs <- resize (siz - 1) arbitrary
                return $ MkOb $ ob : obs

check_pb1 (MkOb ob) =
    not (null ob)
    ==> pullback (id (ft ob)) ob == ob

check_pb2 (MkOb ob) =
    not (null ob)
    ==> q (id (ft ob)) ob == id ob
