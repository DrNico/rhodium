{-# LANGUAGE
    Arrows, TupleSections,
    Rank2Types,
    MultiParamTypeClasses
  #-}

module Arrow.ParserArrow where

import Prelude ( ($), Either(..), undefined )

import Control.Arrow
import Control.Category

data ParserArrow e a i o = ParserArrow {
        runParser   :: forall r.
                       a e r        -- FailConsumed
                    -> a o r        -- OkSkip
                    -> a e r        -- FailSkip
                    -> a o r        -- OkConsumed
                    -> a i r
    }

instance Category (ParserArrow e a) where
    id      = ParserArrow $ \_ oks _ _ -> oks
    g . f   = ParserArrow $ \fc oks fs okc ->
        let ok = runParser g fc oks fs okc in runParser f fc ok fs ok

instance ArrowApply a => Arrow (ParserArrow e a) where
    arr f   = ParserArrow $ \_ oks _ _ -> oks . arr f
    
    first f = ParserArrow $ \fc oks fs okc -> proc (x,y) -> do
        runParser f fc (oks . arr (,y)) fs (okc . arr (,y)) -<< x

instance (ArrowApply a, ArrowChoice a) => ArrowChoice (ParserArrow e a) where
    left f  = ParserArrow $ \fc oks fs okc -> proc x -> do
        case x of
            Left x  -> runParser f fc (oks . arr Left) fs (okc . arr Left) -< x

instance ArrowApply a => ArrowZero (ParserArrow e a) where
    zeroArrow   = ParserArrow $ \_ _ fs _ -> proc _ -> do
        fs -< undefined

