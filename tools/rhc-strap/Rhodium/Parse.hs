{-# LANGUAGE
    Arrows,
    OverloadedStrings
  #-}

module Rhodium.Parse
where

import Prelude (($), const, (==))

import Arrow.ArrowMany
import Arrow.Lexer
import Rhodium.Types
import Util.Parser

import Control.Arrow
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

type Prod r         = Parser r ()
type Terminal r     = Parser r () ByteString
type Separator r    = Parser r () ()

program :: Prod r Program
program = proc () -> do
    defs <- defblock `sepByL` vskip -< ()
    returnA -< Program defs

defblock :: Prod r (Name, Value)
defblock = proc () -> do
    name <- ident -< ()
    manyws -< ()
    lex_ $ word8 123 -< () -- '{'
    vskip -< ()
    clauses <- clause `sepByL` vskip -< ()
    vskip -< ()
    lex_ $ word8 125 -< () -- '}'
    returnA -< (name, VFun clauses)

clause :: Prod r Clause
clause = proc () -> do
    head <- morphism -< ()
    manyws -< ()
    simple <+> defined -< head
    where
    simple = proc head -> do
        lex_ $ word8 10 -< () -- LF
        pause -< ()
        let Morphism ins name outs = head -- TODO: check that name is ok
        returnA -< Clause ins [] outs
    defined = proc head -> do
        lex_ $ string ":=" -< ()
        vskip -< ()
        lex_ $ word8 91 -< () -- '['
        vskip -< ()
        tail <- morphism `sepByL` vskip -< ()
        vskip -< ()
        lex_ $ word8 93 -< () -- ']'
        let Morphism ins _ outs = head -- TODO: check that name is ok
        returnA -< Clause ins tail outs

morphism :: Prod r Morphism
morphism = proc () -> do
    ins <- pattern `sepByL` somews -< ()
    manyws -< ()
    fname <- (( proc () -> do
        lex_ $ string ">->" -< ()
        returnA -< "@" )
      <+> ( proc () -> do
        lex_ $ string ">-" -< ()
        manyws -< ()
        fname <- ident <+> (lex $ string "@") -< ()
        (lex_ $ string "->") <<< manyws -< ()
        returnA -< fname
        )) -< ()
    manyws -< ()
    outs <- pattern `sepByL` somews -< ()
    returnA -< Morphism ins fname outs

pattern :: Prod r Pat
pattern = proc () ->
        ( do
        var <- ident -< ()
        returnA -< PVar var
        )
    <+> ( do
        ctr <- atom -< ()
        args <- option ( proc _ -> do
            lex_ $ char '(' -< ()
            args <- pattern `sepByL` somews -< ()
            lex_ $ char ')' -< ()
            returnA -< args ) -< []
        returnA -< Pat ctr args
        )


ident :: Terminal r
ident =
    lex $ lower >>> many alphaNum

atom :: Terminal r
atom =
    lex $ upper >>> many alphaNum

-- TODO: handle tabs in horizontal whitespace
manyws :: Separator r
manyws =
    lex_ $ takeWhile (== ' ')

somews :: Separator r
somews =
    lex_ $ some $ char ' '

manyvs :: Separator r
manyvs =
    lex_ $ many $ word8 32 <+> word8 10

vskip :: Separator r
vskip = proc () -> do
    manyws -< ()
    many ((lex_ $ word8 10) >>> pause >>> manyws) -< ()
    returnA -< ()