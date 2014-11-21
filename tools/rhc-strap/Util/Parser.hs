{-# LANGUAGE
    GADTs, Arrows,
    GeneralizedNewtypeDeriving
  #-}

module Util.Parser where

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.State
import Control.Category
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF
import Data.Char
import Data.Monoid

import Arrow.ArrowMany
import Arrow.Lexer
import Arrow.Parser
import Util.Parser.TextPos

newtype Parser r i o = Parser {
        theParser :: ParserArrow (PResult r) ParseError (StateArrow ByteString (->)) i o
                  -- ParserPlate * -> StateArrow { (i,BS) -> (PResult r,BS) }
    } deriving (Category, Arrow, ArrowChoice,
        ArrowZero, ArrowPlus, ArrowMany, ArrowLoop,
        ArrowError ParseError)

type Lexer = LexerArrow () ()
type ParseError = String

data PResult a where
    PDone   :: a -> PResult a
    PFail   :: ParseError -> PResult a
    PMore   :: (ByteString -> (PResult a, ByteString))
            -> PResult a

instance Show a => Show (PResult a) where
    showsPrec p (PDone x)   =
        showParen (p >= 10) (showString "PDone " . showsPrec 11 x)
    showsPrec p (PFail e)   =
        showParen (p >= 10) (showString "PFail " . showsPrec 11 e)
    showsPrec p (PMore _)   =
        showParen (p >= 10) (showString "PMore <...>")

parse :: Parser r () r -> ByteString -> PResult r
parse (Parser p) input =
    fst $ runState
        (runParser p ParserPlate {
            failConsumed    = arr PFail,
            okSkip          = arr PDone,
            failSkip        = arr PFail,
            okConsumed      = arr PDone
        })
        ((),input)

-- | Feed additional input to a partial result.
feed :: ByteString -> PResult r -> PResult r
feed input (PMore f) =
    fst $ f input
feed _ res = res

lex_ :: LexerArrow () e -> Parser r () ()
lex_ theLexer = Parser $ ParserArrow $ \plate ->
    StateArrow $ 
        let this = \((),s) ->
                    case lexer theLexer ((),s) of
                        LDone (_,s') ->
                            if BS.length s == BS.length s'
                            then runState (okSkip plate) ((),s')
                            else runState (okConsumed plate) ((),s')
                        LFail e ->
                            if BS.null s
                            then (PMore $ \s' -> this ((),s'), s)
                            else runState (failSkip plate) (e,s)
        in this

-- | Insert the given lexer, capturing whatever it matches.
lex :: LexerArrow () e -> Parser r () ByteString
lex theLexer = Parser $ ParserArrow $ \plate ->
    StateArrow $ 
        let this = \((),s) ->
                    case lexer theLexer ((),s) of
                        LDone (_,s') ->
                            let len = BS.length s - BS.length s'
                            in  if len == 0
                                then runState (okSkip plate) (BS.empty, s')
                                else runState (okConsumed plate) (BS.take len s, s')
                        LFail e ->
                            if BS.null s
                            then (PMore $ \s' -> this ((),s'), s)
                            else runState (failSkip plate) (e,s)
        in this

-- Combinators

-- | Turn a Parser into a look-ahead Parser. A failure occurring after consuming
--   input is recovered from.
--   ## TODO: can be implemented on top of ArrowError class
try :: Parser r i o -> Parser r i o
try p = Parser $ ParserArrow $ \plate ->
    StateArrow $ \(x,s) ->
        runState
            (runParser (theParser p) plate {
                failConsumed = failSkip plate . StateArrow (\(e,_) -> (e,s))
            })
            (x,s)

option :: ArrowPlus a => a i i -> a i i
option try =
    try <+> id

sepByL :: Parser r () i -> Parser r () e -> Parser r () [i]
sepByL pat sep = proc () -> do
    x  <- pat -< ()
    xs <- many $ try (sep >>> (arr $ const ()) >>> pat) -< ()
    returnA -< x : xs

-- | Pause parsing if input stream is empty. This is useful when
--   placed immediately before 'many', preventing the latter from
--   matching the empty string.
pause :: Parser r i i
pause = Parser $ ParserArrow $ \plate ->
    StateArrow $
        let this = \(x,s) ->
                    if BS.null s
                    then (PMore $ \s' -> this (x,s'), s)
                    else runState (okSkip plate) (x,s)
        in this

-- These belong into an ArrowPlus combinator library
{-
many :: ArrowPlus a => a i i -> a i i
many f =
    some f <+> id

some :: ArrowPlus a => a i i -> a i i
some f =
    f >>> many f

sepBy :: ArrowPlus a => a i i -> a () e -> a i i
sepBy pat sep =
    pat >>> many ( proc x -> do
                sep -< ()
                pat -< x
            )

manyL :: (ArrowPlus a, ArrowLoop a) => a () i -> a () [i]
manyL f = proc () -> do
    some (accumL f) <+> id -< []

someL :: (ArrowPlus a, ArrowLoop a) => a () i -> a () [i]
someL f = proc () -> do
    rec
        ys <- (accumL f) -< xs
        xs <- manyL f -< ()
    returnA -< ys

accumL :: Arrow a => a () i -> a [i] [i]
accumL f = proc xs -> do
    x <- f -< ()
    returnA -< x : xs
-}
-- Internals
