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

-- ## TODO: check precondition
--   ft ob == target f
pullback :: HomC -> ObC -> ObC
pullback f ob =
    (subst (morph f) (head ob)) : (source f)

-- ## TODO: check precondition
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


-----
-- QuickCheck
-----

-- Test the equations
-- 1)    pullback (id (ft ob)) ob == ob
-- 2)    q (id (ft ob)) ob == id ob
-- 3)    proj (target $ q f ob) . q f ob == f . proj (pullback f ob)
--    pullback (g . f) ob == pullback g (pullback f ob)
--    q (g . f) ob == q g (pullback f ob) . q f ob


check_pb1 (MkOb ob) = not (null ob)
    ==> pullback (id (ft ob)) ob == ob

check_pb2 (MkOb ob) = not (null ob)
    ==> q (id (ft ob)) ob == id ob

-- arguments of 'pullback f ob' must satisfy
--   ft ob == target f
{-
check_pb3 (MkHom f) =
    let extract (Left _) = False
        extract (Right b) = b
        test :: Either String Bool = do
            MkOb [o] <- resize 1 arbitrary -- ## TODO: should also generate variables
            let ob = o : (target f)
            lft <- (proj $ target $ q f ob) . (q f ob)
            rgt <- f . (proj $ pullback f ob)
            return $ lft == rgt
    in do
        extract (test ob)
-}
-- Instances

newtype GenOb = MkOb {
        unMkOb :: [Term Int]
    }
    deriving (Show)

instance Arbitrary GenOb where
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
                        return $ Pred atom []  -- ## TODO: add arguments to predicates
                    ]
                MkOb obs <- resize (siz - 1) arbitrary
                return $ MkOb $ ob : obs

-- create an arbitrary term that can appear at the head of a list of 
-- terms forming an Object
genObN :: Int -> Gen (Term Int)
genObN 0 = do
    letter <- choose ('a','z')
    return $ Pred [letter] []
genObN n = sized term'
    where
    term' 0 = oneof [ do
                i <- choose (0,n - 1)
                return $ Var i
            , do
                letter <- choose ('a','z')
                return $ Pred [letter] []
            ]
    term' m = oneof [ do
                i <- choose (0,n - 1)
                return $ Var i
            , do
                letter <- choose ('a','f')
                nargs <- choose (0, m `div` 2)
                MkOb args <- resize nargs arbitrary
                return $ Pred [letter] args
            ]

newtype GenHom = MkHom {
        unMkHom :: HomC
    }
    deriving (Show)

{-
instance Arbitrary GenHom where
    arbitrary = sized $ \siz -> do
        srcsiz <- choose (0,siz-1)
        let tgtsiz = siz - srcsiz
        MkOb src <- resize srcsiz arbitrary
        MkOb tgt <- resize tgtsiz arbitrary
        MkOb m <- resize tgtsiz arbitrary
        return $ MkHom $ HomC {
                source = src,
                target = tgt,
                morph  = m  -- ## TODO: build a type-correct morphism
            }
-}
-- generates two composable Morphisms
-- instance Arbitrary (GenHom,GenHom) where
