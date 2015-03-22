{-# LANGUAGE
    GADTs, ScopedTypeVariables, BangPatterns
  #-}

{-|
Module          : Data.ListN
Description     : Lists indexed with a Type-level length
Copyright       : (c) Nicolas Godbout, 2015
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : Experimental
-}
module Data.ListN where

import Data.Naturals (Zero, Succ)

import Data.Functor (Functor(..))
import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid(..))

import Prelude ((.), Eq(..), Bool(..), (&&), Int, (+), (-), (<), otherwise, error)

data ListN n a where
    Nil             :: ListN Zero a
    Cons            :: a -> ListN n a -> ListN (Succ n) a

head :: ListN (Succ n) a -> a
head (Cons x _) = x

init :: ListN (Succ n) a -> ListN n a
init (Cons (x :: a) r) = go x r
    where
    go :: forall n. a -> ListN n a -> ListN n a
    go x Nil = Nil
    go x (Cons y r) = Cons x (go y r)

length :: ListN n a -> Int
length = foldl' (+) 0 . ((<$) 1)

(!!) :: ListN n a -> Int -> a
(xs :: ListN n a) !! n
    | n < 0 = error "ListN.!!: negative index\n"
    | otherwise = go n xs
        where
            go :: forall n. Int -> ListN n a -> a
            go _ Nil = error "ListN.!!: index too large\n"
            go 0 (Cons x _) = x
            go n (Cons _ r) = go (n - 1) r

snoc :: ListN n a -> a -> ListN (Succ n) a
snoc r (x :: a) = go x r
    where
    go :: forall n. a -> ListN n a -> ListN (Succ n) a
    go x Nil = Cons x Nil
    go x (Cons y r) = Cons y (go x r)

zipWith :: (a -> b -> c) -> ListN n a -> ListN n b -> ListN n c
zipWith (f :: a -> b -> c) = go
    where
    go :: forall n. ListN n a -> ListN n b -> ListN n c
    go Nil Nil = Nil
    go (Cons x r) (Cons y s) = Cons (f x y) (go r s)

instance Eq a => Eq (ListN n a) where
    Nil == Nil              = True
    Cons a as == Cons b bs  = a == b && as == bs
    _ == _                  = False

instance Functor (ListN n) where
    fmap (f :: a -> b) = go
        where
        go :: forall n. ListN n a -> ListN n b
        go Nil = Nil
        go (Cons y ys) = Cons (f y) (go ys)

    (<$) (x :: a) = go
        where
        go :: forall n b. ListN n b -> ListN n a
        go Nil = Nil
        go (Cons _ ys) = Cons x (go ys)

instance Foldable (ListN n) where
    fold = go
        where
        go :: forall n a. (Monoid a => ListN n a -> a)
        go Nil = mempty
        go (Cons y ys) = y `mappend` go ys

    foldr (f :: a -> b -> b) z = go
        where
        go :: forall n. ListN n a -> b
        go Nil = z
        go (Cons y ys) = y `f` go ys

    foldMap (f :: a -> m) = go
        where
        go :: forall n. ListN n a -> m
        go Nil = mempty
        go (Cons y ys) = f y `mappend` go ys

    foldl (f :: b -> a -> b) z = go z
        where
        go :: forall n. b -> ListN n a -> b
        go x Nil = x
        go x (Cons y ys) = go (f x y) ys

    foldl' (f :: b -> a -> b) z = go z
        where
        go :: forall n. b -> ListN n a -> b
        go x Nil = x
        go x (Cons y ys) = let !z = f x y in go z ys

{-  TODO: rewrite rules
    "init/snoc" forall list x. init (snoc list x) = list
  -}
