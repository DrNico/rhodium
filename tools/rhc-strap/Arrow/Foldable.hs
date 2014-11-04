{-# LANGUAGE Arrows, MultiParamTypeClasses, FlexibleInstances #-}

module Arrow.Foldable (
        Foldable(..)
    )
where

import Prelude ( ($), uncurry )

import Arrow.Functor

import Control.Arrow
import Control.Category
import Data.Monoid

class Arrow a => Foldable t a where
    fold        :: Monoid m => a (t m) m
    fold =
        foldr (arr $ uncurry mappend) mempty
    
    foldr       :: a (x,y) y -> y -> a (t x) y
    foldl       :: a (y,x) y -> y -> a (t x) y


instance ArrowChoice a => Foldable [] a where
    foldr f y0 = go
        where
        go = proc xs ->
            case xs of
                []      ->
                    returnA -< y0
                x:xs    -> do
                    y <- go -< xs
                    f -< (x,y)
    
    foldl f y0 = proc xs ->
        go -< (y0,xs)
        where
        go = proc (y0,xs) ->
            case xs of
                []      ->
                    returnA -< y0
                x:xs    -> do
                    y <- f -< (y0,x)
                    go -< (y,xs)


-- toStream :: Foldable t a => a (t x) (Stream x)
-- toStream = 
    
