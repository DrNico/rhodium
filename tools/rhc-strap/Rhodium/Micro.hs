
module Rhodium.Micro where

import Data.List        (find)
import Data.Maybe       (fromJust)

data Program = Program [ADef]
data ADef    = ADef Name [Clause]
type Clause  = ([Pat], AExpr, [Pat])
data AExpr   = ACall [Pat] Name [Pat] | AId

data Pat     = Pat Name [Pat] | PVar Name
type Name    = String
type Binds   = [(Name,Value)]

type Stack   = [Value]
data Value   = Value Name [Value] deriving (Show)   -- Pattern without variable


-- Transform input stack to output stack with given task
--   imperative style
run :: Program -> Clause -> (Binds, Stack) -> Maybe (Binds, Stack)
run prog ([], AId, []) bs =
    Just bs

run prog (pats, AId, ctrs) (binds,stack) = do
    -- pull values from the stack, defining local variables in 'pats'
    (bs,st) <- unifyAll pats stack
    
    -- push values on the stack, replacing all variables in 'ctrs'
    return (binds, map (// (bs ++ binds)) ctrs ++ st)

run prog (pats, ACall ins fname outs, ctrs) (binds,stack) = do
    -- pull values from the stack, defining local variables in 'pats'
    (bs1,st1) <- unifyAll pats stack
    
    -- lookup function name, crash if undefined
    let ADef _ clauses = aDef prog fname
    
    -- push values on the stack according to 'ins'
    let st2 = map (// (bs1 ++ binds)) ins ++ st1
    
    -- try the function clauses in sequence, use the first match
    -- presenting the stack and bindings
    (_, st3):_ <- swap $ map (\c -> run prog c (binds,st2)) clauses
    
    -- pull values from the stack according to 'outs'
    (bs4, st4) <- unifyAll outs st3
    
    -- push values on the stack, using local and global bindings
    let outbinds = bs4 ++ binds
    return (outbinds, map (// outbinds) ctrs ++ st4)


unifyAll :: [Pat] -> Stack -> Maybe (Binds, Stack)
unifyAll [] vals   = Just ([], vals)
unifyAll (p:pats) (v:vals) =
    case (unify p v, unifyAll pats vals) of
        (Just b, Just (bs, vals)) -> Just (b ++ bs, vals)
        _                         -> Nothing

unify :: Pat -> Value -> Maybe Binds
unify (PVar name) val   = Just [(name,val)]
unify (Pat pname pats) (Value vname vals)
    | pname == vname    = fmap concat $ swap $ map (uncurry unify) (unzip_1 (pats,vals))
    | otherwise         = Nothing

(//) :: Pat -> Binds -> Value
(PVar name) // binds =
    fromJust $ lookup name binds
(Pat name pats) // binds =
    Value name (map (// binds) pats)

-- lookup arrow definition in the program
aDef :: Program -> Name -> ADef
aDef (Program prog) name =
    fromJust $ find p prog
    where p (ADef n _) = n == name

-- some datatypes

true  = Value "True" []
false = Value "False" []

int2Value :: Int -> Value
int2Value 0 = Value "Z" []
int2Value n = Value "S" [int2Value $ n - 1]

value2Int :: Value -> Int
value2Int (Value "Z" [])  = 0
value2Int (Value "S" [n]) = (value2Int n) + 1

zero  = Value "Z" []
one   = Value "S" [zero]
two   = Value "S" [one]
three = Value "S" [two]

prog1 = Program [
    ADef "add" [
        (   [Pat "Z" [], PVar "y"]
        ,   AId
        ,   [PVar "y"]
        ) ,
        (   [Pat "S" [PVar "x"], PVar "y"]
        ,   ACall [PVar "x", PVar "y"] "add" [PVar "r"]
        ,   [Pat "S" [PVar "r"]]
        )
    ] ,
--     ADef "mult" [
--         (   [Pat "Z" [], PVar "y"]
--         ,   AId
--         ,   [Pat "Z" []]
--         ) ,
--         (   [Pat "S" [PVar "x"], PVar "y"]
--         ,   
--     ] ,
    ADef "even" [
        (   [Pat "Z" []]
        ,   AId
        ,   [Pat "True" []]
        ) ,
        (   [Pat "S" [PVar "x"]]
        ,   ACall [PVar "x"] "odd" [PVar "0"]
        ,   [PVar "0"]
        )
    ] ,
    ADef "odd" [
        (   [Pat "Z" []]
        ,   AId
        ,   [Pat "False" []]
        ) ,
        (   [Pat "S" [PVar "x"]]
        ,   ACall [PVar "x"] "even" [PVar "0"]
        ,   [PVar "0"]
        )
    ]
    ]
   



-- Helper functions

-- this should be an instance of Swap [] Maybe
swap :: [Maybe a] -> Maybe [a]
swap []             = Just []
swap (Nothing : xs) = swap xs
swap (Just x : xs)  = fmap ((:) x) (swap xs)

-- inverse of unzip, a zip expecting lists of equal length
unzip_1 :: ([a],[b]) -> [(a,b)]
unzip_1 (a:as , b:bs) = (a,b) : unzip_1 (as,bs)
unzip_1 ([]   , []  ) = []
