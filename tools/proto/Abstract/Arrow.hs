{-# LANGUAGE
    TypeFamilies, PolyKinds
  #-}

{-|
Module:         Abstract.Arrow
Description:    Dual definition of Arrows
Copyright:      (c) 2015 Nicolas Godbout
Licence:        BSD-3
-}
module Abstract.Arrow where

import Control.Category
import Data.Monoid (Monoid(..))

import Data.Typeable (Proxy(..))

import Prelude (($), undefined)


-- coarr :: ContArrow b f a b -> f a b
-- coarr f == runCont f id
newtype ContArrow f z a b = ContArrow {
    runCont :: f b z -> f a z
}

-- coarr :: ReaderArrow b r f a b -> f (a,r) b
-- coarr f == runReader f id
newtype ReaderArrow r z a b = ReaderArrow {
    runReader :: (b -> z) -> r -> a -> z
              -- f b z -> f (a,r) z
}

-- coarr :: WriterArrow b w f a b -> f a (b,w)
-- coarr f == runWriter f id
newtype WriterArrow w z a b = WriterArrow {
    runWriter :: (w -> b -> z) -> a -> z
}

-- coarr :: StateArrow b s f a b -> f (a,s) (b,s)
-- coarr f == runState f id
newtype StateArrow s z a b = StateArrow {
    runState :: (s -> b -> z) -> s -> a -> z
}

-- coarr :: ErrorArrow b e f a b -> f a (Either e b)
-- coarr f == runError f (arr Left) (arr Right)
newtype ErrorArrow e z a b = ErrorArrow {
    runError :: (e -> z) -> (b -> z) -> a -> z
}

{-| Arrow of commutative monoidal categories

This class is equivalent to the one from the Arrow package, but has
different class members. This rewriting has two big advantages:
  * In arrow transformer instances, it is no longer necessary for the
    inner arrow to support the intended outer transformation. Inner
    arrows no longer need to be instances of ArrowApply. An outer
    ArrowChoice instance can now be defined even if the inner
    arrow is not.
  * An Arrow instance can now observe every variable binding, even
    when using Haskell do/proc notation which generates opaque
    functions through @arr f@.
    This observation is obtained through the @push@ and @pull@ methods.
    Arrows towers no longer need to be based on (->). This allows
    using Haskell do notation to generate AST for DSLs.
-}
class Category f => Arrow f where
    pull :: (a -> f x y) -> f (x,a) y
    push :: a -> f x (x,a)
    swap :: f (a,b) (b,a)

instance Arrow (->) where
    pull f = \(x,a) -> f a x
    push a = \x -> (x,a)
    swap   = \(x,y) -> (y,x)

arr :: Arrow f => (a -> b) -> f a b
arr f = drop . (first (arr f)) . (push ())
    where drop = pull (\_ -> id)

first :: Arrow f => f a b -> f (a,c) (b,c)
first f = pull (\a -> push a . f)

second :: Arrow f => f a b -> f (c,a) (c,b)
second f = swap . first f . swap

instance Arrow f => Arrow (ContArrow f z) where
    pull f = ContArrow $ \h ->
                pull (\c -> runCont (f c) h)

    push c = ContArrow $ \h -> h . push c

    swap   = ContArrow $ \h -> h . swap


instance Category (ContArrow f z) where
    id = ContArrow $ id

    g . f = ContArrow $ \h ->
                runCont f (runCont g h)

instance Category (ReaderArrow r z) where
    id = ReaderArrow $ \f _ -> f

    g . f = ReaderArrow $ \h r ->
                runReader f (runReader g h r) r

instance Monoid w => Category (WriterArrow w z) where
    id = WriterArrow $ \f -> f mempty

    g . f = WriterArrow $ \h ->
                runWriter f (\w -> runWriter g (\v -> h (mappend w v)))

instance Category (StateArrow s z) where
    id = StateArrow $ id

    g . f = StateArrow $ \h s ->
                runState f (runState g h) s

instance Category (ErrorArrow e z) where
    id = ErrorArrow $ \_ f -> f

    g . f = ErrorArrow $ \e h ->
                runError f e (runError g e h)
