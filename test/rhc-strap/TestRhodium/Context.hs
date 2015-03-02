
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


check_pb1 ob@(ObC ts) = not (null ts)
    ==> pullback (id (ft ob)) ob == ob

check_pb2 ob@(ObC ts) = not (null ts)
    ==> q (id (ft ob)) ob == id ob

check_pb3 :: HomC -> Gen Bool
check_pb3 f = do
    o <- genObN $ length (morph f)
    let ob = ObC $ o : (target f)
    return $ (proj $ ObC $ target $ q f ob) . (q f ob) == f . (proj $ pullback f ob)

-- Instances

instance Arbitrary ObC where
    arbitrary = sized $ \siz -> fmap ObC $ gen siz (siz `div` 2)
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
                ObC args <- resize nargs arbitrary  -- ## MOD: carry 'm'
                return $ Pred [letter] args
            ]

instance Arbitrary HomC where
    arbitrary = sized $ \siz -> do
        srcsiz <- choose (0,siz)
        let tgtsiz = siz - srcsiz
        ObC src <- resize srcsiz arbitrary
        ObC tgt <- resize tgtsiz arbitrary
        ObC m <- resize tgtsiz arbitrary -- ## MOD: generate morphisms
        return $ HomC {
                source = src,
                target = tgt,
                morph  = m  -- ## TODO: build a type-correct morphism
            }

-- generates two composable Morphisms
-- instance Arbitrary (GenHom,GenHom) where
