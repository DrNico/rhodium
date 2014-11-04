{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where

import Prelude hiding ( concat, getLine, putStr, putStrLn, readFile )

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.ByteString ( ByteString, snoc )
import Data.ByteString.Char8 ( unpack, concat, getLine, putStr, putStrLn, readFile )
import Data.String ( IsString(fromString) )

import Micro
import Parse
import Types


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
        Fail _ _ err -> do
            putStrLn $ fromString err
            repl env
        Done rest res ->
--            putStrLn $ "Warning: ignored \"" ++ rest
            process res env
        partial ->
            more partial
    where
    more cont = do
        putStr "... "
        line <- getLine
        case feed cont (snoc line 10) of
            Fail _ _ err -> do
                putStrLn $ fromString err
                repl env
            Done rest res ->
--                putStrLn $ "Warning: ignored \"" ++ rest
                process res env
            partial ->
                more partial

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
    case parseOnly program file of
        Left err -> do
            putStrLn $ fromString err
            repl (prog,binds)
        Right (Program newdefs) -> do
            let Program defs = prog
            repl (Program $ newdefs ++ defs,binds)
            
process Quit env = return env
    
command :: Parser Command
command =
        ( do
            string "#show"
            somews
            name <- ident
            manyws
            word8 10
            return $ Show name
        )
    <|> ( do
            string "#load"
            somews
            name <- ident
            return $ Load name
        )
    <|> ( do
            string "#quit"
            return Quit
        )
    <|> ( do
            def <- defblock
            return $ Def def
        )
    <|> ( do
            mor <- morphism
            return $ Expr mor
        )

motd :: [ByteString]
motd = [
    "==================================\n",
    "\xC2\xB5Rh, the micro-Rhodium interpreter\n"
    ]