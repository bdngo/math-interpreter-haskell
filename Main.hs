module Main (
    main
) where

import Control.Exception ( try, SomeException (SomeException) )
import System.Environment ( getArgs )
import Lexer ( lexer )
import Parser ( shuntingYard )
import Interpreter ( interpret )

loop :: Bool -> IO ()
loop pf = do
    res <- try (do
    putStr "calc > "
    txt <- getLine
    let tokens = lexer txt
    if null tokens
        then loop pf
        else if pf
            then print $ interpret tokens
            else print $ interpret $ shuntingYard tokens
    loop pf) :: IO (Either SomeException ())
    case res of
        Left e -> do
            print e
            loop pf
        Right _ -> loop pf

main :: IO ()
main = do
    argv <- getArgs
    loop ("--postfix" `elem` argv)
