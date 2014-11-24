
module Rhodium.Check where

import Rhodium.Types

type Context = [(Name, Type)]

data Type =
    TVar Name
  | TAtom
  | TFun [Type] [Type]
  deriving (Eq, Show)

-- | Check that all definitions in the Program are valid:
--   * clauses have consistent arity and type
--   * variables are defined before use
--   * arrows are used with correct arity and type
check :: Program -> Maybe CError
check (Program defs) =

-- Context is
--   * a spaghetti-stack of symbol dictionaries
--   * a stack

data Context = Context t {
        binds   :: [(Label, t)],
        stack   :: [t]
    }

checkMorphism :: Context (Morphism ALabel VLabel) -> 
checkMorphism ctx = runCokleisli $ Cokleisli $ proc m -> do
    fun <- lookup ALabel
    

checkMorphism :: Program -> Morphism
              -> (Binds, Stack) -> Either String (Binds, Stack)
checkMorphism prog (Morphism ins fname outs) (binds,stack) = do
    let Program defs = prog
    
    -- lookup function name
    --   resolved name is: 1) local ref to Value; 2) global def
    Just fun <-
            flip $ map Right $ lookup fname binds
        `mplus`
            flip $ map Right $ lookup fname defs
        `mplus` (
            raise $ "Not in scope: '" ++ show fname ++ "'" )
    
    -- push values on the stack according to 'ins'
    let st2 = map (// (binds ++ defs)) ins ++ stack
    
    -- unify the stack with the function type
    (_, st3):_ <- 
    
    -- pull values from the stack according to 'outs'
    (bs4, st4) <- unifyAll outs st3
    
    return (bs4 ++ binds, st4)

flip :: Maybe (Either a b) -> Either a (Maybe b)
flip Just (Right x) = Right (Just x)
flip Just (Left x)  = Left x
flip Nothing        = Left undefined
