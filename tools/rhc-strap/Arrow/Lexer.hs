{-# LANGUAGE
    GADTs,
    MultiParamTypeClasses, FlexibleInstances,
    GeneralizedNewtypeDeriving
  #-}

module Arrow.Lexer where

import Prelude (
        ($), Char, Bool, (||), String,
        Eq(..), Ord(..), Maybe(..), Show(..), (++)
    )

import Control.Applicative
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.State
import Control.Category
import Control.Monad
import Data.ByteString
import qualified Data.ByteString.UTF8 as UTF
import Data.Char
import Text.Show

-- Implementation notes:
--  add a capture strategy

-- | The @LexerArrow@ efficiently matches the head of the input string.
-- It does not, however, capture the match results. This should be performed
-- by the caller, typically an outer Arrow.
newtype LexerArrow i o = LexerArrow {
        runLexer :: StateArrow ByteString (Kleisli LResult) i o
                 -- (i, BS) -> LResult (o, BS)
    } deriving (Category, Arrow, ArrowChoice, ArrowApply, ArrowZero, ArrowPlus)

data LResult a where
    LDone   :: a -> LResult a
    LFail   :: String -> LResult a
    LMore   :: (ByteString -> LResult a) -> LResult a

instance Show a => Show (LResult a) where
    showsPrec p (LDone x)   =
        showParen (p >= 10) (showString "LDone " . showsPrec 11 x)
    showsPrec p (LFail e)   =
        showParen (p >= 10) (showString "LFail " . showsPrec 11 e)
    showsPrec p (LMore _)   =
        showParen (p >= 10) (showString "LMore <...>")

lexer :: LexerArrow i o -> (i,ByteString) -> LResult (o,ByteString)
lexer lex =
    runKleisli $ runState $ runLexer lex

(<?>) :: LexerArrow i o -> String -> LexerArrow i o
lex <?> e =
    lex <+> (LexerArrow $ StateArrow $ Kleisli $ \_ -> LFail e)

infix 0 <?>

-- Functions

alpha :: LexerArrow i i
alpha = satisfy isAlpha <?> "alpha"

digit :: LexerArrow i i
digit = satisfy isDigit <?> "digit"

space :: LexerArrow i i
space = satisfy isSpace <?> "space"

lower :: LexerArrow i i
lower = satisfy isLower <?> "lower-case letter"

upper :: LexerArrow i i
upper = satisfy isUpper <?> "upper-case letter"

char :: Char -> LexerArrow i i
char c = satisfy (== c) <?> "character '" ++ (c : "'")

newline :: LexerArrow i i
newline = (satisfy $ \c -> (c == '\n') || (c == '\r')) <?> "newline"

satisfy :: (Char -> Bool) -> LexerArrow i i
satisfy test = LexerArrow $ StateArrow $ Kleisli $ cont test
    where
    cont test = \(x,s) ->
        case UTF.uncons s of
            Just (c,s') ->
                if test c then LDone (x,s') else LFail ""
            Nothing ->
                LMore $ \input -> cont test (x,input)

takeWhile :: (Char -> Bool) -> LexerArrow i i
takeWhile test = LexerArrow $ StateArrow $ Kleisli $ cont test
    where
    cont test = \(x,s) ->
        let (_,s') = UTF.span test s
        in  if null s'
            then LMore $ \input -> cont test (x,input)
            else LDone (x,s')

string :: ByteString -> LexerArrow i i
string str = LexerArrow $ StateArrow $ Kleisli $ cont str
    where
    cont str = \(x,s) ->
        if str `isPrefixOf` s
            then LDone (x, drop (length str) s)
            else if length s < length str
                 then LMore $ \input -> cont str (x,input)
                 else LFail $ "string: " ++ show str


-- Instance declarations

instance ArrowError String LexerArrow where
    raise   = LexerArrow $ StateArrow $ Kleisli $ \(e,_) -> LFail e
    
--    tryInUnless try succ err =

instance Functor LResult where
    fmap f (LDone a)    = LDone (f a)
    fmap _ (LFail s)    = LFail s
    fmap f (LMore g)    = LMore (g >=> LDone . f)

instance Applicative LResult where
    pure    = return
    (<*>)   = ap

instance Monad LResult where
    return  = LDone
    
    (LDone a)   >>= f     = f a
    (LMore g)   >>= f     = LMore (g >=> f)
    (LFail s)   >>= _     = LFail s

instance Alternative LResult where
    empty   = mzero
    (<|>)   = mplus

instance MonadPlus LResult where
    mzero           = LFail "mzero"
    
    (LDone a) `mplus` _   = LDone a
    (LMore f) `mplus` g   = LMore $ \x -> f x `mplus` g
    (LFail s) `mplus` g   = g
