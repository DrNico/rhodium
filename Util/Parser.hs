{-# LANGUAGE
    Arrows,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
    GeneralizedNewtypeDeriving
  #-}

module Util.Parser
    ( Parser, Lexer, parse, lex )
where

import Prelude (
    ($), ($!), Bool(..), Char, Int, String, Eq(..), Maybe(..), Either(..), Show, fromInteger,
    not, elem, (>), (+), (-), error, const, uncurry, undefined )

-- Our Arrows
-- import Arrow.Arrow
import Arrow.ArrowPlus
import Arrow.Foldable
import Arrow.Functor
import Arrow.Monad
import Arrow.Parser

import Util.Parser.Lexer
import Util.Parser.TextPos

import Control.Arrow ( Arrow(..), ArrowChoice(..), returnA )
import Control.Arrow.Operations ( ArrowState(..) )
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import Control.Category
import Data.Monoid

import Data.ByteString.Internal ( ByteString(..) )
import Data.ByteString.Short    ( ShortByteString, toShort, empty )


type ParseError = ByteString

parse :: ArrowChoice a => Parser a () r -> a ByteString (Either ParseError r)
parse (Parser p) = proc str -> do
    (res,_) <- (runState $ runParser p) -< ((),ParseState str mempty)
    case res of
        OkConsumed r        -> returnA -< Right r
        OkSkip r            -> returnA -< Right r
        FailConsumed e      -> returnA -< Left e
        FailSkip e          -> returnA -< Left e

-- lift a simple lexer into the parsing arrow
lex :: ArrowChoice a => Lexer -> Parser a () ShortByteString
lex (Lexer lexer) = proc _ -> do
    str <- fetchStr -< ()
    let (res,str') = (runState $ runParser lexer) (mempty,str)
    case res of
        OkConsumed r     -> do
            loc <- fetchLoc -< ()
            storeLoc -< loc `mappend` r
            let PS pload offset  len = str
            let PS _     offset' len' = str'
            storeStr -< str'
            okConsumed -< toShort $ PS pload offset (len - len')
        OkSkip r        -> do
            okSkip -< empty
        FailConsumed e      ->
            failConsumed -< e
        FailSkip e          ->
            failSkip -< e


-----
-- Internals
-----

newtype Parser a i o = Parser {
        theParser :: ParserArrow ParseError (StateArrow ParseState a) i o
    }
    deriving (Category, Arrow, ArrowChoice, ArrowState ParseState)

data ParseState = ParseState {
        stream      :: ByteString,
        location    :: TextPos
    }

-- these are useful internally in lieu of returnA
okSkip          :: Arrow a => Parser a r r
okConsumed      :: Arrow a => Parser a r r
failSkip        :: Arrow a => Parser a ParseError r
failConsumed    :: Arrow a => Parser a ParseError r

okSkip          = Parser $ ParserArrow $ arr OkSkip
okConsumed      = Parser $ ParserArrow $ arr OkConsumed
failSkip        = Parser $ ParserArrow $ arr FailSkip
failConsumed    = Parser $ ParserArrow $ arr FailConsumed

-- parse state modifiers
fetchStr        :: ArrowChoice a => Parser a () ByteString
storeStr        :: ArrowChoice a => Parser a ByteString ()
fetchLoc        :: ArrowChoice a => Parser a () TextPos
storeLoc        :: ArrowChoice a => Parser a TextPos ()

fetchStr = proc _ -> do
    st <- fetch -< ()
    returnA -< stream st
storeStr = proc str -> do
    st <- fetch -< ()
    store -< st { stream = str }

fetchLoc = proc _ -> do
    st <- fetch -< ()
    returnA -< location st
storeLoc = proc loc -> do
    st <- fetch -< ()
    store -< st { location = loc }

