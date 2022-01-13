module Main (
    main
) where

import Control.Exception ( try, SomeException (SomeException) )
import Control.Monad ( when )
import System.Environment ( getArgs )
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )

import Lexer ( lexer )
import Parser ( parse, shuntingYard )
import Interpreter ( interpretTree, interpretStack )

loop :: [String] -> IO ()
loop argv = 
    let pf = "--postfix" `elem` argv
        pt = "-t" `elem` argv
        pa = "-a" `elem` argv
        sy = pf || (not pf && "--shunting" `elem` argv)
    in do
    res <- try (do
    putStr "calc > "
    txt <- getLine
    let tokens = lexer txt
    when pt (print tokens)
    when (null tokens) (loop argv)
    let toInterp = if sy then Left (shuntingYard tokens) else Right (parse tokens)
    when (not pf && pa) (print toInterp)
    print (either interpretStack interpretTree toInterp)
    loop argv) :: IO (Either SomeException ())
    case res of
        Left e -> do
            print e
            loop argv
        Right _ -> loop argv

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- hack for IO
    argv <- getArgs
    loop argv
