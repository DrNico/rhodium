{-# LANGUAGE
    TypeFamilies, MultiParamTypeClasses, FlexibleInstances
  #-}

module Abstract.Category where

import Control.Applicative
import Control.Arrow (Kleisli(..), (<<<))
import Data.Typeable

import qualified Prelude as Prelude
import Prelude (($), zipWith, Monad(..), map, fmap)

-- | Class of Categories.
--   Instances should satisfy the following equations:
--     * if @target f == source g@, then @g . f@ is well-defined
--     * if @source f == x@, then @f . id x == f@
--     * if @target f == x@, then @id x . f == f@
--     * if @h . g . f@ is well-defined, then @(h . g) . f == h . (g . f)@
class Category hom where
    type Ob hom     :: * -> *

    source          :: hom a b -> Ob hom a
    target          :: hom a b -> Ob hom b

    id              :: Ob hom a -> hom a a
    (.)             :: hom b c -> hom a b -> hom a c

instance Category (->) where
    type Ob (->) = Proxy

    source _ = Proxy
    target _ = Proxy

    id _ = Prelude.id
    (.)  = (Prelude..)

instance (Monad m) => Category (Kleisli m) where
    type Ob (Kleisli m) = Proxy

    source _ = Proxy
    target _ = Proxy

    id _ = Kleisli return
    (.)  = (<<<)

instance (Category cat, Applicative f) => Category (Ap2 f cat) where
    type Ob (Ap2 f cat) = Ap1 f (Ob cat)

    source (Ap2 f) = Ap1 (fmap source f)
    target (Ap2 f) = Ap1 (fmap target f)

    id (Ap1 x) = Ap2 (fmap id x)

    Ap2 g . Ap2 f =
        Ap2 $ pure (.) <*> g <*> f

newtype Ap1 f g a = Ap1 {
    unAp1 :: f (g a)
}
newtype Ap2 f g a b = Ap2 {
    unAp2 :: f (g a b)
}

-----
-- Functor between Categories
-----

class Functor c d where
    type OMap c d a :: *
    type FMap c d a b :: *

    omap  :: (Category c, Category d)
          => (Ob c) a -> OMap c d a
    mmap  :: (Category c, Category d)
          => c a b -> FMap c d a b

instance (Category cat, Applicative f) => Functor cat (Ap2 f cat) where
    type OMap cat (Ap2 f cat) a = Ap1 f (Ob cat) a
    type FMap cat (Ap2 f cat) a b = Ap2 f cat a b

    omap x = Ap1 (pure x)
    mmap f = Ap2 (pure f)

