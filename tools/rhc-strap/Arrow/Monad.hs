{-# LANGUAGE Arrows,
    MultiParamTypeClasses, FlexibleInstances #-}

module Arrow.Monad (
        Monad(..), (>=>), (<=<)
    )
where

import Prelude ()

-- import Arrow.Arrow
import Arrow.Functor

import Control.Arrow
import Control.Category
import qualified Control.Monad as M
import System.IO ( IO )

import Data.List ( concat )

class Monad m a where
    inject      :: a i (m i)
    join        :: a (m (m i)) (m i)

-- Arrow-compatible version of monad composition
(>=>) :: (Category a, Functor m a, Monad m a)
      => a b (m c) -> a c (m d) -> a b (m d)
f >=> g =
    f >>> map g >>> join

infixr 1 >=>

(<=<) :: (Category a, Functor m a, Monad m a)
      => a c (m d) -> a b (m c) -> a b (m d)
g <=< f =
    f >>> map g >>> join

infixr 1 <=<


instance Arrow a => Monad [] a where
    inject      = arr (\x -> [x])
    join        = arr concat

instance Arrow a => Monad IO a where
    inject      = arr (M.return)
    join        = arr (\m -> m M.>>= id)

{-
instance (Arrow a, M.Monad m) => Monad m a where
    inject      = arr M.return
    bind        = arr $ \m -> m M.>>= id
-}