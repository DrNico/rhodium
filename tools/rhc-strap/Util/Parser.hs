{-# LANGUAGE
    GADTs,
    GeneralizedNewtypeDeriving
  #-}

module Util.Parser where

import Prelude hiding (id,(.), length)

import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.State
import Control.Category
import Data.ByteString ( ByteString, length )
import qualified Data.ByteString.UTF8 as UTF
import Data.Char
import Data.Monoid

import Arrow.Lexer
import Arrow.Parser
import Util.Parser.TextPos

newtype Parser r i o = Parser {
        theParser :: ParserArrow r ParseError LexerArrow i o
    } deriving (Category, Arrow, ArrowChoice, ArrowZero, ArrowPlus)

type Lexer = LexerArrow () ()
type ParseError = String

data PResult a where
    PDone   :: a -> PResult a
    PFail   :: ParseError -> PResult a
    PMore   :: (b,ByteString) -> (b -> LResult a)
            -> PResult a


parse :: Parser r () r  -> ByteString -> LResult r
parse (Parser p) input =
    fmap fst $ runKleisli 
        (runState $ runLexer $ runParser p ParserPlate {
                failConsumed    = raise,
                okSkip          = id,
                failSkip        = raise,
                okConsumed      = id
        })
        ((),input)


-- | Feed additional input to a partial result.
feed :: ByteString -> LResult r -> LResult r
feed input (LMore f) =
    f input
feed _ res = res

-- | Insert the given lexer, capturing whatever it matches.
-- lex   :: LexerArrow () () -> Parser () ByteString
-- lex theLexer =


-- | Insert the given lexer, ignore the matched sub-string.

lex_ :: Lexer -> Parser r () ()
lex_ theLexer = Parser $ ParserArrow $ \plate ->
        LexerArrow $ StateArrow $ Kleisli $ cont plate
    where
    cont :: ParserPlate String LexerArrow () r
         -> ((),ByteString)
         -> LResult (r,ByteString)
    cont plate = \(_,s) -> cases plate s $ lexer theLexer ((),s)

    cases plate s res = case res of
        LDone r@(_,s') ->
            if length s == length s'  -- change to internal offset comparison
            then lexer (okSkip plate) r
            else lexer (okConsumed plate) r
        LFail e ->
            lexer (failSkip plate) (e,s)
        LMore f ->
            LMore $ \input -> cases plate s $ f input
            
    
-- Internals
type Plate = ParserPlate String 
