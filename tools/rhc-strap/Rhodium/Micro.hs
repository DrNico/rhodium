{-# LANGUAGE
    OverloadedStrings
  #-}

module Rhodium.Micro where

import Prelude hiding ( flip )

import Control.Monad    (mplus)
import Data.List        (find)
import Data.Maybe       (fromJust)

import Rhodium.Types

-- Transform input stack to output stack with given task
--   imperative style
--   note: 'main' can only have a single clause
runClause :: Program -> Clause -> (Binds, Stack) -> Maybe (Binds, Stack)
runClause prog (Clause pats mors ctrs) (binds,stack) = do
    -- pull values from the stack, defining local variables in 'pats'
    (bs1,st1) <- unifyAll pats stack
    
    -- run the morphisms
    (bs2,st2) <- chain
        (map (\m -> runMorphism prog m) mors)
        (bs1 ++ binds,st1)
    
    -- push values on the stack, using local and global bindings
    let outbinds = bs2 ++ binds
    return (outbinds, map (// outbinds) ctrs ++ st2)

-- Evaluate a single morphism in the given environment
runMorphism :: Program -> Morphism -> (Binds, Stack) -> Maybe (Binds, Stack)
runMorphism prog (Morphism ins fname outs) (binds,stack) = do
    let Program defs = prog
    
    -- lookup function name, crash if undefined
    --   resolved name is: 1) local ref to Value; 2) global def
    clauses <- ( do
            VFun cs <- lookup fname binds
            return cs )
        `mplus` ( do
            VFun cs <- lookup fname defs
            return cs )
        `mplus` (
            error $ "Arrow lookup failure: " ++ show fname )
    
    -- push values on the stack according to 'ins'
    let st2 = map (// (binds ++ defs)) ins ++ stack
    
    -- try the function clauses in sequence, use the first match
    -- presenting the stack and bindings
    (_, st3):_ <- flip $ map (\c -> runClause prog c (binds,st2)) clauses
    
    -- pull values from the stack according to 'outs'
    (bs4, st4) <- unifyAll outs st3
    
    return (bs4 ++ binds, st4)


unifyAll :: [Pat] -> Stack -> Maybe (Binds, Stack)
unifyAll [] vals   = Just ([], vals)
unifyAll (p:pats) (v:vals) =
    case (unify p v, unifyAll pats vals) of
        (Just b, Just (bs, vals)) -> Just (b ++ bs, vals)
        _                         -> Nothing
unifyAll _ _ = Nothing

unify :: Pat -> Value -> Maybe Binds
unify (PVar name) val   = Just [(name,val)]
unify (Pat pname pats) (Value vname vals)
    | pname == vname    = fmap concat $ flip $ map (uncurry unify) (unzip_1 (pats,vals))
    | otherwise         = Nothing

(//) :: Pat -> Binds -> Value
(PVar name) // binds =
    fromJust $ lookup name binds
(Pat name pats) // binds =
    Value name (map (// binds) pats)

-- some datatypes

int2Value :: Int -> Value
int2Value 0 = Value "Z" []
int2Value n = Value "S" [int2Value $ n - 1]

value2Int :: Value -> Int
value2Int (Value "Z" [])  = 0
value2Int (Value "S" [n]) = (value2Int n) + 1

-- Helper functions

chain :: Monad m => [a -> m a] -> a -> m a
chain (f:fs) x =
    f x >>= \y -> chain fs y
chain [] x =
    return x

-- this should be an instance of Flip [] Maybe
flip :: [Maybe a] -> Maybe [a]
flip []             = Just []
flip (Nothing : xs) = flip xs
flip (Just x : xs)  = fmap ((:) x) (flip xs)

-- inverse of unzip, a zip expecting lists of equal length
unzip_1 :: ([a],[b]) -> [(a,b)]
unzip_1 (a:as , b:bs) = (a,b) : unzip_1 (as,bs)
unzip_1 ([]   , []  ) = []
