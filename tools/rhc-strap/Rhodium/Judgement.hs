
module Rhodium.Judgement where



-----
-- Judgements
-----

data Judgement var
  = JUpTerm (UpTerm var)
  | JDnTerm (DnTerm var) (UpTerm var)
  | JUpEq (UpTerm var) (UpTerm var)
  | JDnEq (DnTerm var) (DnTerm var) (UpTerm var)
  deriving (Eq)

data Context var = Context {
    object      :: [UpTerm var],
    upBinds     :: [(Label, TypeInCtx)], -- broken 'var', hardwired as 'Int' here
    dnBinds     :: [(Label, TermInCtx)]
}

-- | Check if second UpTerm in context matches the first UpTerm in its context,
--   and if it does, return the second UpTerm in context with possibly
--   modified bindings.
type TypeInCtx = ([UpTerm Int],UpTerm Int)
upUnifies :: TypeInCtx -> TypeInCtx
          -> Bool
upUnifies = undefined

type TermInCtx = ([UpTerm Int],DnTerm Int,UpTerm Int)
dnUnifies :: TermInCtx -> TermInCtx
          -> Bool
dnUnifies = undefined

-- | Check that the given Context is valid.
validCtx :: Context Int -> Bool
validCtx ctx
    | [] <- object ctx
        = True
    | o:os <- object ctx
        =  validCtx ctx { object = os }
        && entails ctx { object = os } (JUpTerm o)

-- | Obtain the type of the given Term in Context
typeOf :: Context Int -> DnTerm Int -> UpTerm Int
typeOf ctx (DnVar n) =
    object ctx !! n
--typeOf ctx (DnPred s ts) =
--    let (pctx,ptrm,ptyp) = lookup s (dnBinds ctx)
--    in undefined
--    -- ## TODO
typeOf ctx (DnLambda f) =
    error "impossible to infer the type of a naked lambda"
--typeOf ctx (DnApp k x) =
--    let b:a:gamma = object ctx
--        ctx' = ctx { object = a:gamma }
--    in typeOf ctx' b
typeOf ctx (DnPair x y) =
    let a = typeOf ctx x
        b = typeOf ctx y
    in UpSigma a b

-- | Check that the supplied Context, assumed to be valid, entails the given Judgement.
entails :: Context Int -> Judgement Int -> Bool
-- Implementation notes: this function requires lookup capabilities into the Context,
-- of both variables and atoms. To make the function generic, the Context should
-- be the member of a class providing these lookups.

-- `A type` judgements
entails ctx (JUpTerm UpType) =
    error "slippery slope towards Girard's paradox"
entails ctx (JUpTerm (UpVar n)) =
    (object ctx !! n) == UpType
    -- ^ variable designates a type
entails ctx (JUpTerm (UpPred s ts)) =
    case lookup s (upBinds ctx) of
        Just typ_ctx
            -> upUnifies typ_ctx (object ctx, UpPred s ts)
            -- ^ the atom is defined and its arguments are well-typed
        Nothing
            -> False
            -- ^ the atom is undefined in Context
entails ctx (JUpTerm (UpPi a b)) =
    entails ctx { object = a:(object ctx) } (JUpTerm b)
    -- ^ Pi-FORM rule
entails ctx (JUpTerm (UpSigma a b)) =
    let gamma = object ctx
    in entails ctx (JUpTerm a)
    && entails ctx { object = a:gamma } (JUpTerm b)
    -- ^ Sigma-FORM rule

-- `a : A` judgements
entails ctx (JDnTerm (DnVar n) typ) =
    entails ctx (JUpEq (object ctx !! n) typ)
    -- ^ VBLE rule
entails ctx (JDnTerm (DnPred s ts) typ) =
    case lookup s (dnBinds ctx) of
        Just trm_ctx
            -> dnUnifies trm_ctx (object ctx, DnPred s ts, typ)
        Nothing
            -> False
    -- ^ VBLE rule over atoms
entails ctx (JDnTerm (DnLambda f) (UpPi a b)) =
    let ctx' = ctx { object = a:(object ctx) }
    in entails ctx' (JUpTerm b)
    && entails ctx' (JDnTerm f b)
    -- ^ Pi-INTRO rule
entails ctx (JDnTerm (DnApp k x) typ) =
    let b:a:gamma = object ctx
        ctx' = ctx { object = gamma }
    in entails ctx' (JDnTerm x a)
    && entails ctx' (JDnTerm k (UpPi a typ))
    -- ^ Pi-APP rule
entails ctx (JDnTerm (DnPair x y) (UpSigma a b)) =
    let cb:ca:gamma = object ctx
    in  entails ctx {object = gamma} (JUpTerm a)
    &&  entails ctx {object = ca:gamma} (JUpTerm b)
    -- ^ Sigma-INTRO rule
    -- ## FIXME: where do x and y appear ?

-- Type Equality Judgements
entails ctx (JUpEq (UpVar n1) (UpVar n2)) =
    n1 == n2
entails ctx (JUpEq (UpPred s1 ts1) (UpPred s2 ts2)) =
    s1 == s2 &&
    and (zipWith (\t1 t2 -> entails ctx (JUpEq t1 t2)) ts1 ts2)
entails ctx (JUpEq (UpPi a1 b1) (UpPi a2 b2)) =
    entails ctx (JUpEq a1 a2) && entails ctx (JUpEq b1 b2)
entails ctx (JUpEq (UpSigma a1 b1) (UpSigma a2 b2)) =
    entails ctx (JUpEq a1 a2) && entails ctx (JUpEq b1 b2)

-- Term Equality Judgements
entails ctx (JDnEq (DnVar n1) (DnVar n2) typ) =
    n1 == n2
entails ctx (JDnEq (DnPred s1 ts1) (DnPred s2 ts2) typ) =
    if  s1 == s2 && and (zipWith (\t1 t2 -> entails ctx (JDnEq t1 t2 typ)) ts1 ts2)
    then entails ctx (JDnTerm (DnPred s1 ts1) typ)
    else False  -- ## TODO: check for aliases
entails ctx (JDnEq (DnLambda f1) (DnLambda f2) typ) =
    if entails ctx (JDnEq f1 f2 typ)
    then entails ctx (JDnTerm f1 typ)
    else False -- ## TODO: check for aliases
entails ctx (JDnEq (DnPair a1 b1) (DnPair a2 b2) typ) =
    if entails ctx (JDnEq a1 a2 typ) && entails ctx (JDnEq b1 b2 typ)
    then entails ctx (JDnTerm a1 typ)
    else False -- ## TODO: try harder

