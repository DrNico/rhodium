{-# LANGUAGE
    OverloadedStrings, Arrows
  #-}

module Main where

import Prelude hiding ( concat, getLine, putStr, putStrLn, readFile, lookup )

import Control.Arrow
import Control.Comonad (($>))
import Data.ByteString ( ByteString, snoc )
import Data.ByteString.Char8 ( unpack, concat, getLine, putStr, putStrLn, readFile )
import Data.String ( IsString(fromString) )

import Arrow.Lexer
import Rhodium.Eval
-- import Rhodium.Micro
import Rhodium.Parse
import Rhodium.Types
import Util.Parser


main :: IO ()
main = do
    putStrLn $ concat motd
    repl $ Context [] [] ()
    return ()

type Env = Context Value ()

repl :: Env -> IO Env
repl env = do
    putStr "\x1B[2m>>> \x1B[0m"
    line <- getLine
    case parse command (snoc line 10) of
        PFail err -> do
            putStrLn $ fromString err
            repl env
        PDone res ->
            process res env
        PMore f ->
            more f
    where
    more cont = do
        putStr "... "
        line <- getLine
        case fst $ cont (snoc line 10) of
            PFail err -> do
                putStrLn $ fromString err
                repl env
            PDone res ->
                process res env
            PMore f ->
                more f

data Command =
    Expr Morphism
  | Def (Name, Value)
  | Load ByteString
  | Show ByteString
  | Dump
  | Quit

process :: Command -> Env -> IO Env
process (Def def) (Context bs st ()) =
    repl $ Context (def:bs) st ()

process (Expr mor) ctx =
    case evalMorphism (ctx $> mor) of
        Just ctx@(Context _ [] _) ->
            repl ctx
        Just _ -> do
            putStrLn $ "Error: uncaptured output arguments."
            repl ctx
        Nothing -> do
            putStrLn $ "Fail."
            repl ctx

process (Show name) ctx =
    case lookup name ctx of
        Just v -> do
            showRh v
            repl ctx
        Nothing -> do
            putStrLn $ "Error: undefined variable."
            repl ctx

process (Load name) ctx = do
    file <- readFile $ unpack name
    let res  = parse program file
        loop = \x -> case x of
                        PFail err -> do
                            putStrLn $ fromString err
                            repl ctx
                        PDone (Program newdefs) -> do
                            repl ctx { binds = newdefs ++ (binds ctx) }
                        PMore f ->
                            loop $ fst $ f "\x04"
     in loop res

process Dump ctx@(Context bs _ _) = do
    showRh $ Program bs
    repl ctx

process Quit env = return env
    
command :: Prod r Command
command =
        ( proc () -> do
            lex_ $ string "#show" -< ()
            somews -< ()
            name <- ident -< ()
            manyws -< ()
            lex_ $ word8 10 -< ()
            returnA -< Show name
        )
    <+> ( proc () -> do
            lex_ $ string "#load" -< ()
            somews -< ()
            name <- ident -< ()
            manyws -< ()
            lex_ $ word8 10 -< ()
            returnA -< Load name
        )
    <+> ( proc () -> do
            lex_ $ string "#quit\n" -< ()
            returnA -< Quit
        )
    <+> ( proc () -> do
            lex_ $ string "#dump\n" -< ()
            returnA -< Dump
        )
    <+> ( proc () -> do
            def <- defblock -< ()
            returnA -< Def def
        )
    <+> ( proc () -> do
            mor <- morphism -< ()
            returnA -< Expr mor
        )

motd :: [ByteString]
motd = [
    "==================================\n",
    "\xC2\xB5Rh, the micro-Rhodium interpreter\n"
    ]