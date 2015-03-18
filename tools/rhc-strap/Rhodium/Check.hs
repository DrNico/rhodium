
module Rhodium.Check where


import Rhodium.Contextual

import Control.Monad.Error.Class


-- | (Up-)Check that the given Morphism of the Contextual Category indeed has
--   a type compatible with the source and target.
validHom :: HomC -> Context ()
validHom f@(HomC _ [] [])
	= return ()
validHom (HomC _ (typ:_) [])
	= throwError $ strMsg $
		"arity error: type lacks an accompanying term\n" -- ++ getLoc typ
validHom f = do
	let trm:trms = morph f
	let typ:typs = target f
	case trm of
		TmVar n ->
		TmAtom a as ->
		TmType t ->
		TmLambda k ->
		TmApp k x ->



newtype Context a = Context {
	runContext :: StateT Bindings (EitherT [String])
} deriving (Monad, MonadError, MonadState)

data Bindings = Bindings {
		typeBinds	:: Map Label HomC,
		termBinds	:: Map Label HomC
	}


-- ## TODO: introduce a 'Checked' Monad, tagging terms that have been type-checked.
-- pure  :: ObC -> Checked ObC
-- id    :: Checked ObC -> Checked HomC
-- <^.^> :: HomC -> HomC -> Checked HomC
-- <.^>  :: Checked HomC -> HomC -> Checked HomC
-- <^.>  :: HomC -> Checked HomC -> Checked HomC
-- <.>   :: Checked HomC -> Checked HomC -> Checked HomC

-- | Composition of morphisms in a Contextual Category.
(<.>) :: (Error e, MonadError e m)
    => HomC -> HomC
    -> m HomC
g <.> f =
    if target f == source g
    then return HomC {
            source = source f,
            target = target g,
            morph = fmap (subst $ morph f) (morph g)
        }
    else throwError $ strMsg "error"

