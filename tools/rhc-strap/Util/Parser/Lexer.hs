
module Util.Parser.Lexer (
        Lexer, LexerArrow(..),
        alpha, digit, space, lower, upper, alphaNum, char, satisfy
    )
where

import Arrow.ArrowPlus
import Arrow.Parser
import Util.Parser.TextPos

import Control.Arrow ( Arrow(..) )
import Control.Arrow.Transformer.State ( StateArrow(..) )
import Control.Category
import Data.ByteString ( ByteString, isPrefixOf, drop )
import qualified Data.ByteString.UTF8   as UTF
import Data.Char
    ( isSpace, isUpper, isLower, isAlphaNum, isAlpha, isDigit, isHexDigit )
import Data.Monoid ( Monoid(..) )
import Prelude ( ($), Bool, Char, Eq(..), Maybe(..) )


type Lexer = LexerArrow () ()
type ParseError = ByteString

data LexerArrow i o =
    Lexer {
        runLexer :: ParserArrow ParseError (StateArrow ByteString (->)) TextPos TextPos
    }

instance Category LexerArrow where
    id      = Lexer $ id
    g . f   = Lexer $ runLexer g . runLexer f

instance ArrowZero LexerArrow where
    zero = Lexer $ ParserArrow $ arr $ \_ -> FailSkip mempty

instance ArrowPlus LexerArrow where
    (Lexer f) <+> (Lexer g) = Lexer $ f <+> g



alpha :: Lexer
alpha = satisfy isAlpha

digit :: Lexer
digit = satisfy isDigit

space :: Lexer
space = satisfy isSpace

lower :: Lexer
lower = satisfy isLower

upper :: Lexer
upper = satisfy isUpper

alphaNum :: Lexer
alphaNum = satisfy isAlphaNum

char :: Char -> Lexer
char c = satisfy ((==) c)

newline :: Lexer


satisfy :: (Char -> Bool) -> Lexer
satisfy test =
    Lexer $ ParserArrow $ StateArrow $ \(pos,str) ->
        case UTF.uncons str of
            Just (c,str') | test c ->
                (OkConsumed $ pos `mappend` TextPos 0 1, str')
            _ ->
                (FailSkip $ UTF.fromString "", str)

string :: ByteString -> Lexer
string s =
    Lexer $ ParserArrow $ StateArrow $ \(pos,str) ->
        case s `isPrefixOf` str of
            True ->
                let len = length s in case len == 0 of
                    True ->
                        (OkSkip pos,str)
                    False ->
                        (OkConsumed pos `mappend` stringPos s, drop len str)
            False ->
                (FailSkip $ UTF.fromString "", str)


stringPos :: ByteString -> TextPos
stringPos s = go s mempty
    where
    go s pos = case uncons s of
        Nothing     ->  pos
        Just (c,s') ->
            go s' (pos `mappend` charPos c)

charPos :: Char -> TextPos
charPos c =
    if c == '\n' || c == '\r' then TextPos 1 0 else TextPos 0 1
