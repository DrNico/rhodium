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

import Prelude ((.), Eq(..), Bool(..), (&&), Int, (+))

data ListN n a where
    Nil             :: ListN Zero a
    Cons            :: a -> ListN n a -> ListN (Succ n) a

init :: ListN (Succ n) a -> ListN n a
init (Cons x Nil) = Nil
init (Cons x (Cons y ys)) = Cons x (init (Cons y ys))

length :: ListN n a -> Int
length = foldl' (+) 0 . ((<$) 1)


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

