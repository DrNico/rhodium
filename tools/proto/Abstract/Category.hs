{-# LANGUAGE
	TypeFamilies
  #-}

module Abstract.Category where

class Category cat where
	type Ob cat   	:: * -> *
	type Hom cat  	:: * -> * -> *

	source      	:: Hom cat a b -> Ob cat a
	target      	:: Hom cat a b -> Ob cat b

	id          	:: Ob cat a -> Hom cat a a
	(.)				:: Hom cat b c -> Hom cat a b -> Hom cat a c

