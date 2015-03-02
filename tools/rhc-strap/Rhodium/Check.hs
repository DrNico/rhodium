
module Rhodium.Check where

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

