module Main (
    main
) where

import Lexer ( lexer )
import Parser ( shuntingYard )
import Control.Exception ( try, SomeException (SomeException) )

loop :: IO ()
loop = do
    res <- try (do
    putStr "calc > "
    txt <- getLine
    let tokens = lexer txt
    if null tokens
        then loop
        else print (shuntingYard tokens)
    loop) :: IO (Either SomeException ())
    case res of
        Left e -> do
            print e
            loop
        Right _ -> loop

main :: IO ()
main = loop
