{-# LANGUAGE
    Arrows,
    OverloadedStrings
  #-}

module Rhodium.Parse
where

import Prelude (($), const)

-- import Arrow.ArrowPlus
import Rhodium.Types
import Util.Parser
-- import Util.Parser.Lexer

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

type Prod       = Parser * (->) ()
type Terminal   = Parser * (->) () ByteString
type Separator  = Parser * (->) () ()

morphism :: Prod Morphism
morphism = proc () -> do
    ins <- pattern `sepBy` somews -< ()
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
        args <- option ( proc () -> do
            lex_ $ char '(' -< ()
            args <- pattern `sepBy` somews -< ()
            lex_ $ char ')' -< ()
            returnA -< args ) -< []
        returnA -< Pat ctr args
        )


ident :: Terminal
ident =
    lex $ lower >>> many alphaNum

atom :: Terminal
atom =
    lex $ upper >>> many alphaNum

-- TODO: handle tabs in horizontal whitespace
manyws :: Separator
manyws =
    lex_ $ many $ char ' '

somews :: Separator
somews =
    lex_ $ some $ char ' '

manyvs :: Separator
manyvs =
    lex_ $ many $ char ' ' <+> char '\n' <+> char '\r'

