{-# LANGUAGE
    OverloadedStrings, Arrows
  #-}

module Main where

import Prelude hiding ( concat, getLine, putStr, putStrLn, readFile )

import Control.Arrow
import Data.ByteString ( ByteString, snoc )
import Data.ByteString.Char8 ( unpack, concat, getLine, putStr, putStrLn, readFile )
import Data.String ( IsString(fromString) )

import Arrow.Lexer
import Rhodium.Micro
import Rhodium.Parse
import Rhodium.Types
import Util.Parser


main :: IO ()
main = do
    putStrLn $ concat motd
    repl (Program [],[])
    return ()

type Env = (Program, Binds)

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
  | Quit

process :: Command -> Env -> IO Env
process (Def def) (Program defs,binds) =
    repl (Program $ def:defs, binds)

process (Expr mor) (prog,binds) =
    case runMorphism prog mor (binds,[]) of
        Just (binds', []) ->
            repl (prog,binds')
        Just _ -> do
            putStrLn $ "Error: uncaptured output arguments."
            repl (prog,binds)
        Nothing -> do
            putStrLn $ "Fail."
            repl (prog,binds)

process (Show name) (prog,binds) =
    case lookup name binds of
        Just v -> do
            showRh v
            repl (prog,binds)
        Nothing -> do
            putStrLn $ "Error: undefined variable."
            repl (prog,binds)

process (Load name) (prog,binds) = do
    file <- readFile $ unpack name
    case parse program of
        PFail err -> do
            putStrLn $ fromString err
            repl (prog,binds)
        PDone (Program newdefs) -> do
            let Program defs = prog
            repl (Program $ newdefs ++ defs,binds)
--        PMore f -> f "\EOF" and check
            
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