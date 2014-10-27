{-# LANGUAGE Arrows, MultiParamTypeClasses,
        FlexibleInstances, FlexibleContexts,
        UndecidableInstances
  #-}

module Arrow.Functor (
        Functor(..), Swap(..), Zip(..), Unzip(..), Flip(..)
    )
where


import Prelude ( ($), uncurry, undefined )

-- import Arrow.Arrow

import Control.Arrow ( Arrow(..), ArrowChoice(..), returnA )
import Control.Category
import Data.Maybe ( Maybe(..) )
import Data.Monoid
import Data.Either ( Either(..) )
import qualified Data.List as List

----- Functor

-- almost 'Unzip f a'
class Functor f a where
    map         :: a i o -> a (f i) (f o)

class Applicative f a where
    app     :: f (a i o) -> a (f i) (f o)

instance (Arrow a, ArrowChoice a) => Functor [] a where
    map f = proc xs ->
        case xs of
            x:xs    -> do
                y  <- f -< x
                ys <- map f -< xs
                returnA -< y : ys
            []      ->
                returnA -< []

instance (Arrow a, ArrowChoice a) => Functor Maybe a where
    map f = proc x ->
        case x of
            Just r  -> arr (Just) <<< f -< r
            Nothing -> returnA -< Nothing


----- Swap
-- instances should satisfy the following laws:
--      swap . swap  == id
--      map f . swap == swap . map f
class Swap g f where
    swap    :: f (g a) -> g (f a)

instance Swap Maybe []
instance Swap [] Maybe
instance Swap (Either e) []
instance Swap [] (Either e)

----- Zip

class Zip g f where
    zip    :: g (f x) (f y) -> f (g x y)

zipWith :: (Arrow a, Zip g f, Functor f a)
        => a (g x y) z -> a (g (f x) (f y)) (f z)
zipWith f =
    arr zip >>> map f

instance Zip (,) [] where
    zip = uncurry List.zip

instance Zip (,) Maybe where
    zip (Just x, Just y) = Just (x,y)
    zip _                = Nothing

instance Zip (->) [] where
    zip f = List.repeat $ \x -> List.head $ f [x]

-- This is almost ArrowLoop !!
instance Zip (->) ((,) s) where
--  zip :: ((s,x) -> (s,y)) -> (s,x -> y)
    zip f =
        let { f' = \x -> let { (s,y) = f (s,x) ; } in y } in (undefined,f')

------ Unzip

class Unzip g f where
    unzip  :: f (g x y) -> g (f x) (f y)


instance Unzip (,) [] where
    unzip = List.unzip

instance Unzip (->) [] where
    unzip fs = List.zipWith ($) fs

instance Unzip (,) Maybe where
    unzip (Just (x,y)) = (Just x, Just y)
    unzip Nothing      = (Nothing, Nothing)

-- All applicative functors 'f' are instances of 'Unzip (->) f'
instance Monoid s => Unzip (->) ((,) s) where
    unzip (u,f) = \(v,x) -> (u `mappend` v, f x)

----- Flip

class Flip g f where
    flip :: f (g a b) (g c d) -> g (f a c) (f b d)

-- instances of
--  Flip a (,)          ==> first, second
--  Inject a (->)       ==> arr
--  Flip a Either       ==> left, right
--  Extract a Either    ==> fanin
-- are Arrows

instance Flip (->) (,) where
    flip (f,g) = \(x,y) -> (f x,g y)

-- this will NOT typecheck in Haskell, requires dependent types
--  specifically, 
instance Flip (->) Either where
    flip :: Either (a -> a) (b -> c) -> (Either a b -> Either a c)
    flip (Right f) = \x -> case x of
        Right x -> Right $ f x
        Left x  -> Left x
    flip (Left f) = \x -> case x of
        Right x -> Right x
        Left x  -> Left $ f x

instance Flip Either (->) where
    flip 

instance Unzip (->) (Either a) where
    unzip (Right f) = \x -> case x of
        Right x  -> Right $ f x
        Left x   -> Left x
    unzip (Left _)  = \x -> x

instance Arrow a => Flip (,) a where
    flip (f,g) = f *** g

{-
instance Arrow a => Flip a (,) where
    flip (f *** g) = (f,g)

instance ArrowChoice a => Flip Either a where
    flip (Right f) =  f +++ id
    flip (Left g)  = id +++ g

instance ArrowChoice a => Flip a Either where
    flip (f +++ g) =
        Right g
      + Left f
-}

----- FunctorArrow

newtype FunctorArrow f a i o = FunctorArrow {
        runFunctor :: a (f i) (f o)
    }

instance Category a
    => Category (FunctorArrow f a)
    where
    id          = FunctorArrow $ id
    g . f       = FunctorArrow $ runFunctor g . runFunctor f

instance (Arrow a, Functor f (->), Zip (,) f, Unzip (,) f)
    => Arrow (FunctorArrow f a)
    where
    arr f       = FunctorArrow $ arr $ map f
    first f     = FunctorArrow $ arr unzip >>> (first $ runFunctor f) >>> arr zip
    second f    = FunctorArrow $ arr unzip >>> (second $ runFunctor f) >>> arr zip
    f *** g     = FunctorArrow $
        arr unzip >>> (runFunctor f *** runFunctor g) >>> arr zip

{-
instance (Arrow a, ArrowChoice a, Functor f (->), Zip Either f, Unzip Either f)
    => ArrowChoice (FunctorArrow f a)
    where
    f +++ g     = FunctorArrow $
        arr unzip >>> (runFunctor f +++ runFunctor g) >>> arr zip
    f ||| g     = FunctorArrow $
        arr unzip >>> (runFunctor f ||| runFunctor g)
-}