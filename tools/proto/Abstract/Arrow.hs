{-# LANGUAGE
	TypeFamilies
  #-}

{-|
Module: 		Abstract.Arrow
Description:	Dual definition of Arrows
Copyright:		(c) 2015 Nicolas Godbout
Licence:		BSD-3
-}
module Abstract.Arrow where

import Control.Category
import Data.Monoid (Monoid(..))

import Data.Typeable (Proxy(..))

import Prelude (($), undefined)


-- coarr :: ContArrow b f a b -> f a b
-- coarr f == runCont f id
newtype ContArrow z f a b = ContArrow {
	runCont :: f b z -> f a z
}

-- coarr :: ReaderArrow b r f a b -> f (a,r) b
-- coarr f == runReader f id
newtype ReaderArrow r z a b = ReaderArrow {
	runReader :: (b -> z) -> r -> a -> z
			  -- f b z -> f (a,r) z
}

-- coarr :: WriterArrow b w f a b -> f a (b,w)
-- coarr f == runWriter f id
newtype WriterArrow w z a b = WriterArrow {
	runWriter :: (w -> b -> z) -> a -> z
}

-- coarr :: StateArrow b s f a b -> f (a,s) (b,s)
-- coarr f == runState f id
newtype StateArrow s z a b = StateArrow {
	runState :: (s -> b -> z) -> s -> a -> z
}

-- coarr :: ErrorArrow b e f a b -> f a (Either e b)
-- coarr f == runError f (arr Left) (arr Right)
newtype ErrorArrow e z a b = ErrorArrow {
	runError :: (e -> z) -> (b -> z) -> a -> z
}


instance Category f => Category (ContArrow z f) where
	id = ContArrow $ id

	g . f = ContArrow $ \h -> 
				runCont f (runCont g h)

instance Category (ReaderArrow r z) where
	id = ReaderArrow $ \f _ -> f

	g . f = ReaderArrow $ \h r ->
				runReader f (runReader g h r) r

instance Monoid w => Category (WriterArrow w z) where
	id = WriterArrow $ \f -> f mempty

	g . f = WriterArrow $ \h ->
				runWriter f (\w -> runWriter g (\v -> h (mappend w v)))

instance Category (StateArrow s z) where
	id = StateArrow $ id

	g . f = StateArrow $ \h s ->
				runState f (runState g h) s

instance Category (ErrorArrow e z) where
	id = ErrorArrow $ \_ f -> f

	g . f = ErrorArrow $ \e h ->
				runError f e (runError g e h)
