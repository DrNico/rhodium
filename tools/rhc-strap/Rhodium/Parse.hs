{-# LANGUAGE
    Arrows,
    OverloadedStrings
  #-}

module Rhodium.Parse
where

import Prelude (($), const)

import Arrow.ArrowPlus
import Rhodium.Types
import Util.Parser ( Parser, Lexer, parse, lex )
import Util.Parser.Lexer

import Control.Arrow ( Arrow(..), returnA )
import Control.Category
import Data.ByteString ( ByteString )
import Data.Monoid

{- Grammar
    program     ::= defblock*
    defblock    ::= decl '{' NL clause* '}' NL
    clause      ::= morphism (':=' [ morphism* ])?
    morphism    ::= (pat* '>-') id ('->' pat*)
    pat         ::= ident | atom ( '(' pat* ')' )?
    ident       ::= lower alphanum*
    atom        ::= upper alphanum*
-}

type Prod = Parser (->) ()
type Terminal = Parser (->) ()

morphism :: Prod Morphism
morphism = proc () -> do
    ins <- pattern `sepBy` somews -< ()
    manyws -< ()
    fname <- (( proc () -> do
        lex $ string ">->" -< ()
        returnA -< "@" )
      <+> ( proc () -> do
        (lex $ string ">-") -< ()
        manyws -< ()
        fname <- ident <+> (lex $ string "@") -< ()
        (lex $ string "->") <<< manyws -< ()
        returnA -< fname
        )) -< ()
    manyws -< ()
    outs <- pattern `sepBy` somews -< ()
    returnA -< Morphism ins fname outs

pattern :: Prod Pat
pattern = proc () ->
        ( do
        var <- ident -< ()
        returnA -< PVar var
        )
    <+> ( do
        ctr <- atom -< ()
        args <- option [] ( proc () -> do
            lex $ char '(' -< ()
            args <- pattern `sepBy` somews -< ()
            lex $ char ')' -< ()
            returnA -< args ) -< ()
        returnA -< Pat ctr args
        )


ident :: Terminal Name
ident =
    lex $ lower >>> many alphaNum

atom :: Terminal Name
atom =
    lex $ upper >>> many alphaNum

-- TODO: handle tabs in horizontal whitespace
manyws :: Terminal () 
manyws = proc () -> do
    lex $ many $ char ' ' -< ()
    returnA -< ()

somews :: Terminal ()
somews = proc () -> do
    lex $ some $ char ' ' -< ()
    returnA -< ()

manyvs :: Terminal ()
manyvs = proc () -> do
    lex $ many $ char ' ' <+> char '\n' <+> char '\r' -< ()
    returnA -< ()

-- Parser capabilities

sepBy :: (Arrow a, ArrowPlus a) => a () o -> a () e -> a () [o]
sepBy pat sep =
        (many g <<< (arr $ \x -> [x]) <<< pat)
    <+>
        (arr $ const [])
    where
    g = proc x -> do
        sep -< ()
        y <- pat -< ()
        returnA -< x `mappend` [y]
        -- ## this last one is yucky; needs to be turned into a tail call

option :: (Arrow a, ArrowPlus a) => o -> a () o -> a () o
option def try =
    try <+> (arr $ const def)
