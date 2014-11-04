{-# LANGUAGE
    Arrows,
    Rank2Types, TupleSections,
    MultiParamTypeClasses, FlexibleInstances
  #-}

module Arrow.Parser where

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Arrow.Transformer
import Control.Category

data ParserPlate e a o r = ParserPlate {
        failConsumed    :: a e r,
        okSkip          :: a o r,
        failSkip        :: a e r,
        okConsumed      :: a o r
    }

data ParserArrow r e a i o = ParserArrow {
        runParser :: ParserPlate e a o r -> a i r
    }

instance Category (ParserArrow r e a) where
    id      = ParserArrow $ okSkip
    g . f   = ParserArrow $ \plate ->
        let ok = runParser g plate
        in runParser f $ plate { okSkip = ok, okConsumed = ok }
        -- TODO: add shunt to properly handle okConsumed

instance ArrowApply a => Arrow (ParserArrow r e a) where
    arr f   = ParserArrow $ \plate -> okSkip plate . arr f
    
    first f = ParserArrow $ \plate -> proc (x,y) -> do
        runParser f $ plate {
            okSkip = okSkip plate . arr (,y),
            okConsumed = okConsumed plate . arr (,y) } -<< x

instance (ArrowApply a, ArrowChoice a) => ArrowChoice (ParserArrow r e a) where
    left f  = ParserArrow $ \plate -> proc x -> do
        case x of
            Left x  -> runParser f $ plate {
                okSkip = okSkip plate . arr Left,
                okConsumed = okConsumed plate . arr Left } -< x
            Right x -> okSkip plate -< Right x

instance ArrowApply a => ArrowZero (ParserArrow r e a) where
    zeroArrow = ParserArrow $ \plate -> proc _ ->
        failSkip plate -< undefined

instance ArrowApply a => ArrowPlus (ParserArrow r e a) where
    f <+> g = ParserArrow $ \plate -> proc i ->
        runParser f $ plate {
            failSkip = proc _ -> runParser g plate -< i,
            failConsumed = error "Parser failed after consuming input!" } -<< i

instance ArrowApply a => ArrowTransformer (ParserArrow r e) a where
    lift f  = ParserArrow $ \plate -> okSkip plate . f

