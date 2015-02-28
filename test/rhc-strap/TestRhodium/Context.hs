
module TestRhodium.Context where

import Rhodium.Context

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Prelude hiding (id,(.))

testContext :: IO ()
testContext = do
    quickCheck $ label "pullback (id (ft ob)) ob == ob" check_pb1
    quickCheck $ label "q (id (ft ob)) ob == id ob" check_pb2

-- Test the equations
-- 1)    pullback (id (ft ob)) ob == ob
-- 2)    q (id (ft ob)) ob == id ob
-- 3)    proj (target $ q f ob) . q f ob == f . proj (pullback f ob)
-- 4)    pullback (g . f) ob == pullback g (pullback f ob)
-- 5)    q (g . f) ob == q g (pullback f ob) . q f ob


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
    arbitrary = sized $ \siz -> fmap MkOb $ gen siz (siz `div` 2)
        where
        gen m 0 = return []
        gen m n = do
            m <- choose (0, m `div` 2)
            o <- genObN n
            os <- gen m (n - 1)
            return $ o : os


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
                letter <- choose ('a','f')
                return $ Pred [letter] []
            ]
    term' m = oneof [ do
                i <- choose (0,n - 1)
                return $ Var i
            , do
                letter <- choose ('a','f')
                nargs <- choose (0, m `div` 2)
                MkOb args <- resize nargs arbitrary  -- ## MOD: carry 'm'
                return $ Pred [letter] args
            ]

newtype GenHom = MkHom {
        unMkHom :: HomC
    }
    deriving (Show)


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

-- generates two composable Morphisms
-- instance Arbitrary (GenHom,GenHom) where
