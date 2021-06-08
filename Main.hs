module Main (
    main
) where

import Lexer ( lexer )
import Control.Exception ( try, PatternMatchFail )

loop :: IO ()
loop = do
    res <- try (do
    putStr "calc > "
    txt <- getLine
    let tokens = lexer txt
    if null tokens
        then loop
        else print tokens
    loop) :: IO (Either PatternMatchFail ())
    case res of
        Left e -> do
            print e
            loop
        Right _ -> loop

main :: IO ()
main = loop
