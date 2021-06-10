module Main (
    main
) where

import Control.Exception ( try, SomeException (SomeException) )
import Lexer ( lexer )
import Parser ( shuntingYard )
import Interpreter ( interpret )

loop :: IO ()
loop = do
    res <- try (do
    putStr "calc > "
    txt <- getLine
    let tokens = lexer txt
    if null tokens
        then loop
        else print (interpret (shuntingYard tokens))
    loop) :: IO (Either SomeException ())
    case res of
        Left e -> do
            print e
            loop
        Right _ -> loop

main :: IO ()
main = loop
