
module TestRhodium.Context where

import Rhodium.Context

import qualified Control.Exception.Base as Exc
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- test driver, called from 'main'
testContext :: IO ()
testContext = do
    defaultMain tests
    quickCheck $ label "pullback (id (ft ob)) ob == ob" check_pb1
    quickCheck $ label "q (id (ft ob)) ob == id ob" check_pb2


-- Test the equations
-- 1)    pullback (id (ft ob)) ob == ob
-- 2)    q (id (ft ob)) ob == id ob
-- 3)    proj (target $ q f ob) . q f ob == f . proj (pullback f ob)
-- 4)    pullback (g . f) ob == pullback g (pullback f ob)
-- 5)    q (g . f) ob == q g (pullback f ob) . q f ob

-----
-- HUnit
-----

tests = [
        testGroup "Basic operations" []  -- unit, compose commutes, etc.
    ,   testGroup "Pullbacks are strict functors"
        [
            testGroup "diagram 1" testDiagram1
        ]
    ,   testGroup "∏-type structure is coherent"
        [
            testGroup "diagram 2.1" testDiagram2_1
        ]
    ]

-- ob = [A, $1, Pred "B" [$2, $1]]
-- f = x0 : b :- a : A, x0 : a
-- g = x0 : C, x1 : x0 :- h(x1) : b
testDiagram1 =
    let ob = ObC [UpPred "B" [UpVar 1,UpVar 0],UpVar 0, UpPred "A" []]
        f = HomC {
                source = [UpPred "b" []],
                target = [UpVar 0,UpPred "A" []],
                morph = [DnVar 0, DnPred "a" []] }
        g = HomC {
                source = [UpVar 0,UpPred "C" []],
                target = [UpPred "b" []],
                morph = [DnPred "h" [DnVar 0]] }
    in [
        testCase "eq1" $
            pullback (unit (ft ob)) ob @?= ob
    ,   testCase "eq2" $
            q (unit (ft ob)) ob @?= unit ob
    ,   testCase "eq3" $
            proj (ObC $ target $ q f ob) <.> q f ob @?= f <.> proj (pullback f ob)
    ,   testCase "eq4" $
            pullback (f <.> g) ob @?= pullback g (pullback f ob)
    ,   testCase "eq5" $
            q (f <.> g) ob @?= q f ob <.> q g (pullback f ob)
    ]

testDiagram2_1 =
    let obs = [UpPred "D" [],UpPred "C" []]
        ob = ObC $ obs
        a  = HomC {
                source = obs,
                target = (UpPred "A" [UpVar 1]) : obs,
                morph  = (DnPred "p" [DnVar 1,DnVar 0]) : (morph $ unit ob)
            }
        b  = HomC {
                source = target a,
                target = (UpPred "B" [UpVar 1]) : target a,
                morph  = (DnPred "f" [DnVar 0]) : (morph $ unit $ ObC $ target a)
            }
        k = lambda b
        f = HomC {
                target = obs,
                source = [], -- ## TODO
                morph  = []
            }
    in
    Exc.assert (isSection a) $
    Exc.assert (isSection b) $
    [
        testCase "eq1" $
            proj (ObC $ target a) <.> a @?= unit ob
    ,   testCase "eq2" $
            proj (ObC $ target b) <.> app k a @?= a
    ,   testCase "eq3" $
            app k a @?= b <.> a
        -- pullback f ob == Pi (pullback f oba) (pullback f obb)
        -- lambda (qf b) == qf (lambda b)
        -- app (qf k) (qf a) == qf (app k a)
    ]

-----
-- QuickCheck
-----

check_pb1 ob@(ObC ts) = not (null ts)
    ==> pullback (unit (ft ob)) ob == ob

check_pb2 ob@(ObC ts) = not (null ts)
    ==> q (unit (ft ob)) ob == unit ob

check_pb3 :: HomC -> Gen Bool
check_pb3 f = do
    o <- genObN $ length (morph f)
    let ob = ObC $ o : (target f)
    return $ (proj $ ObC $ target $ q f ob) <.> (q f ob)
              == f <.> (proj $ pullback f ob)

-- Instances

instance Arbitrary ObC where
    arbitrary = sized $ \siz -> do
        siz' <- choose (0,siz)
        obs <- resize (siz `div` 2) $ gen siz'
        return $ ObC obs
        where
        gen :: Int -> Gen [UpTerm Int]
        gen 0 = return []
        gen n = do
            o <- genObN (n - 1)
            os <- gen (n - 1)
            return $ o : os


-- create an arbitrary term that can appear at the head of a list of 
-- terms forming an Object
genObN :: Int -> Gen (UpTerm Int)
genObN 0 = do
    letter <- choose ('a','z')
    return $ UpPred [letter] []
genObN n = sized term'
    where
    term' 0 = oneof [ do
                i <- choose (1,n)
                return $ UpVar i
            , do
                letter <- choose ('a','f')
                return $ UpPred [letter] []
            ]
    term' m = oneof [ do
                i <- choose (1,n)
                return $ UpVar i
            , do
                letter <- choose ('a','f')
                nargs <- choose (0, m `div` 2)
                ObC args <- resize nargs arbitrary  -- ## BUG: carry 'n'
                return $ UpPred [letter] args
            ]

{-
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
-}
-- generates two composable Morphisms
-- instance Arbitrary Hom2C where
