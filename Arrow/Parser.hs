{-# LANGUAGE
    Arrows,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances
  #-}

module Arrow.Parser (
        ParserArrow(..), ParseStatus(..)
    )
where

-- Actually, not much special here. This is a generalized Kleisli arrow.

import Control.Arrow ( Arrow(..), ArrowChoice(..), returnA )
import Control.Arrow.Operations ( ArrowState(..) )
import Control.Arrow.Transformer
import Control.Category ( Category(..), (<<<) )
import Prelude ( ($), Either(..), Show, error, undefined )

import Arrow.ArrowPlus
import Arrow.Functor
import Arrow.Monad

data ParseStatus e r =
    OkConsumed r
  | OkSkip r
  | FailConsumed e
  | FailSkip e
    deriving (Show)

data ParserArrow e a i o = ParserArrow {
        runParser :: a i (ParseStatus e o)
    }

-- instances for ParserArrow

instance ArrowChoice a => Category (ParserArrow e a) where
    id      = ParserArrow $ arr OkSkip
    g . f   = ParserArrow $ runParser f >=> runParser g

instance ArrowChoice a => Arrow (ParserArrow e a) where
    arr f   = ParserArrow $ arr (OkSkip <<< f)
    
    first f = ParserArrow $ proc (x,y) -> do
        fx <- runParser f -< x
        case fx of
            OkConsumed r    ->
                returnA -< OkConsumed (r,y)
            OkSkip r        ->
                returnA -< OkSkip (r,y)
            FailConsumed e  ->
                returnA -< FailConsumed e
            FailSkip e      ->
                returnA -< FailSkip e

    second f = ParserArrow $ proc (x,y) -> do
        fy <- runParser f -< y
        case fy of
            OkConsumed r    ->
                returnA -< OkConsumed (x,r)
            OkSkip r        ->
                returnA -< OkSkip (x,r)
            FailConsumed e  ->
                returnA -< FailConsumed e
            FailSkip e      ->
                returnA -< FailSkip e

instance ArrowChoice a => ArrowChoice (ParserArrow e a) where
    left f  = ParserArrow $ proc x -> do
        case x of
            Left x          -> do
                fx <- runParser f -< x
                case fx of
                    OkConsumed r    ->
                        returnA -< OkConsumed (Left r)
                    OkSkip r        ->
                        returnA -< OkSkip (Left r)
                    FailConsumed e  ->
                        returnA -< FailConsumed e
                    FailSkip e      ->
                        returnA -< FailSkip e
            Right x         -> do
                returnA -< OkSkip (Right x)

instance ArrowChoice a => ArrowTransformer (ParserArrow e) a where
    lift f = ParserArrow $ arr OkSkip <<< f

instance (ArrowState s a, ArrowChoice a) => ArrowState s (ParserArrow e a) where
    fetch = lift fetch
    store = lift store

instance ArrowChoice a => ArrowZero (ParserArrow e a) where
    zero  = ParserArrow $ arr $ \_ -> FailSkip undefined

instance ArrowChoice a => ArrowPlus (ParserArrow e a) where
    f <+> g = ParserArrow $ proc x -> do
        fx <- runParser f -< x
        case fx of
            OkConsumed r        -> returnA -< OkConsumed r
            OkSkip r            -> returnA -< OkSkip r
            FailConsumed e      -> arr error -< "Attempt to backtrack after consuming input!"
            FailSkip e          -> runParser g -< x

instance ArrowChoice a => Functor (ParseStatus e) a where
    map f = proc fx ->
        case fx of
            OkConsumed r        -> arr OkConsumed <<< f -< r
            OkSkip r            -> arr OkSkip <<< f -< r
            FailConsumed e      -> returnA -< FailConsumed e
            FailSkip e          -> returnA -< FailSkip e

instance Arrow a => Monad (ParseStatus e) a where
    inject = arr OkSkip
    
    join = arr $ join'
        where
        join' (OkConsumed m)    = case m of
            OkConsumed x        -> OkConsumed x
            OkSkip x            -> OkConsumed x
            FailConsumed e      -> FailConsumed e
            FailSkip e          -> FailConsumed e
        join' (OkSkip m)        = case m of
            OkConsumed x        -> OkConsumed x
            OkSkip x            -> OkSkip x
            FailConsumed e      -> FailConsumed e
            FailSkip e          -> FailSkip e
        join' (FailConsumed e)  = FailConsumed e
        join' (FailSkip e)      = FailSkip e
