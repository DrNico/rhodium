
module Rhodium.Types (
        Program(..)
    ,   Clause(..)
    ,   Morphism(..)
    ,   Pat(..)
    ,   Name
    ,   Binds
    ,   Stack
    ,   Value(..)
    ,   showRh
)
where

import Data.ByteString ( ByteString )
import Data.ByteString.UTF8 ( toString )
import Data.ByteString.Short ( ShortByteString, fromShort )
import Text.PrettyPrint

data Program = Program [(Name,Value)] deriving (Show)

data Clause = Clause [Pat] [Morphism] [Pat] deriving (Show)

data Morphism =
    Morphism [Pat] Name [Pat]
    deriving (Show)

data Pat =
    Pat Name [Pat]
  | PVar Name
  deriving (Show)

type Name = ByteString
         -- ShortByteString

type Binds   = [(Name,Value)]

type Stack   = [Value]
data Value   =
    Value Name [Value]
  | VFun [Clause]
    deriving (Show)

-- pretty printing of Rhodium code
class ToDoc a where
    toDoc :: a -> Doc

instance ToDoc ShortByteString where
    toDoc = text . toString . fromShort

instance ToDoc ByteString where
    toDoc = text . toString

instance ToDoc Pat where
    toDoc (Pat name pats) =
        toDoc name <> if null pats then empty
            else parens (hsep $ map toDoc pats)
    toDoc (PVar name) =
        toDoc name

instance ToDoc Morphism where
    toDoc (Morphism ins f outs) =
        hsep (map toDoc ins) <+> text ">-" <+> toDoc f <+> text "->" <+> hsep (map toDoc outs)

instance ToDoc Clause where
    toDoc (Clause pats [] ctrs) =
        hsep (map toDoc pats) <+> text ">->" <+> hsep (map toDoc ctrs)
    toDoc (Clause pats mors ctrs) =
            (toDoc (Clause pats [] ctrs) <+> text ":=")
        $+$ nest 4 (vcat $ map toDoc mors)

instance ToDoc Program where
    toDoc (Program defs) =
        vcat $ map f defs
        where
        f (name, VFun clauses) =
                toDoc name <+> lbrace
            $+$ nest 4 (vcat $ map toDoc clauses)
            $+$ rbrace
        f (name, v@(Value _ _)) =
            toDoc name <> text " := " <> toDoc v

instance ToDoc Value where
    toDoc (Value name pats) =
        toDoc name <> if null pats then empty
            else parens (hsep $ map toDoc pats)
    toDoc (VFun clauses) =
            lbrace
        $+$ nest 4 (vcat $ map toDoc clauses)
        $+$ rbrace

showRh :: ToDoc a => a -> IO ()
showRh = putStrLn . render . toDoc
        
