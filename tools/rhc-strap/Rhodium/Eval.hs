{-# LANGUAGE OverloadedStrings #-}

module Rhodium.Eval where

import Prelude hiding (flip, lookup)
import Debug.Trace

import Control.Comonad
import Control.Monad (MonadPlus(..))
import qualified Data.List as List

import Rhodium.Types

data Context t e = Context {
        -- TODO: add scoping levels
        binds   :: [(Name, t)],
        stack   :: [t],
        expr    :: e
    }

evalClause :: Context Value Clause -> Maybe (Context Value ())
evalClause ctx = do
    Clause pats mors ctrs <- Just $ extract ctx
    ctx <- pull $ ctx $> pats
    ctx <- chain (map (\m -> \ctx -> evalMorphism $ ctx $> m) mors) ctx
    push $ ctx $> ctrs

evalMorphism :: Context Value Morphism -> Maybe (Context Value ())
evalMorphism ctx = do
    Morphism ins fname outs <- Just $ extract ctx
    VFun clauses <- (lookup fname ctx) `mplus` (
        error $ "Arrow lookup failure: " ++ show fname)
    ctx <- (push $ ctx $> ins) `mplus` (error $ "Undefined variable.")
    ctx <- first $ map (\c -> evalClause $ ctx $> c) clauses
    pull $ ctx $> outs

lookup :: Name -> Context t e -> Maybe t
lookup name (Context bs _ _) =
    List.lookup name bs

-- | Pull values from the stack matching patterns, updating the bindings.
pull :: Context Value [Pat] -> Maybe (Context Value ())
pull ctx@(Context _ _ []) = Just ctx { expr = () }
pull (Context bs (v:vals) (p:pats))
    | Just b <- unify p v =
        pull $ Context (b ++ bs) vals pats
    | otherwise =
        Nothing
pull (Context _ [] _) = error "Empty stack!"

-- | Push values on the stack according to pattern, using current bindings.
push :: Context Value [Pat] -> Maybe (Context Value ())
push ctx@(Context _ _ []) =
    Just ctx { expr = () }
push ctx@(Context _ vals (pat:pats)) = do
    v <- subst $ ctx $> pat
    push $ ctx { stack = v:vals, expr = pats }

unify :: Pat -> Value -> Maybe Binds
unify (PVar name) val   = Just [(name,val)]
unify (Pat pname pats) (Value vname vals)
    | pname == vname    = fmap concat $ flip $ map (uncurry unify) (unzip_1 (pats,vals))
    | otherwise         = Nothing

subst :: Context Value Pat -> Maybe Value
subst ctx =
    case extract ctx of
        (PVar name) ->
            lookup name ctx
        (Pat name pats) -> do
            fmap (Value name) $ flip $ map subst $ distr (ctx $> pats)

distr :: Context t [p] -> [Context t p]
distr (Context b s (e:es)) =
    (Context b s e) : (distr $ Context b s es)
distr (Context _ _ []) = []

-- Instances

instance Functor (Context t) where
    fmap f ctx = ctx { expr = f $ expr ctx }

instance Comonad (Context t) where
    extract         = expr
    duplicate ctx   = ctx { expr = ctx }
    extend f ctx    = ctx { expr = f ctx }

-- Helper functions

chain :: Monad m => [a -> m a] -> a -> m a
chain (f:fs) x =
    f x >>= \y -> chain fs y
chain [] x =
    return x

-- this should be an instance of Flip [] Maybe
flip :: [Maybe a] -> Maybe [a]
flip []             = Just []
flip (Nothing : _)  = Nothing
flip (Just x : xs)  = fmap ((:) x) (flip xs)

first :: [Maybe a] -> Maybe a
first []             = Nothing
first (Nothing : xs) = first xs
first (Just x : _)   = Just x

-- inverse of unzip, a zip expecting lists of equal length
unzip_1 :: ([a],[b]) -> [(a,b)]
unzip_1 (a:as , b:bs) = (a,b) : unzip_1 (as,bs)
unzip_1 ([]   , []  ) = []
