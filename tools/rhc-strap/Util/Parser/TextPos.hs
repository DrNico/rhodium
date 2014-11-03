
module Util.Parser.TextPos (
        TextPos(..)
    )
where

import Data.Monoid

type Line           = Int
type Column         = Int

data TextPos =
    TextPos !Line !Column
    deriving (Eq,Show)

instance Monoid TextPos where
    mempty = TextPos 0 0
    mappend (TextPos l1 c1) (TextPos l2 c2) =
        TextPos
            (l1 + l2)
            (if l2 == 0 then c1+c2 else c2)
