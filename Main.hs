module Main (
    main
) where

import Control.Exception ( try, SomeException (SomeException) )
import Control.Monad ( when )
import System.Environment ( getArgs )
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )
import Lexer ( lexer )
import Parser ( shuntingYard )
import Interpreter ( interpret )

loop :: (Bool, Bool, Bool) -> IO ()
loop (pf, pt, pa) = do
    res <- try (do
    putStr "calc > "
    txt <- getLine
    let tokens = lexer txt
    when pt (print tokens)
    when (null tokens) (loop (pf, pt, pa))
    let toInterp = if pf then tokens else shuntingYard tokens
    when (not pf && pa) (print toInterp)
    print (interpret toInterp)
    loop (pf, pt, pa)) :: IO (Either SomeException ())
    case res of
        Left e -> do
            print e
            loop (pf, pt, pa)
        Right _ -> loop (pf, pt, pa)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- hack for IO
    argv <- getArgs
    loop ("--postfix" `elem` argv, "-t" `elem` argv, "-a" `elem` argv)
