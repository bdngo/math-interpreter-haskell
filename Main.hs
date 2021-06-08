module Main (
    main
) where

import Lexer ( lexer )

loop :: IO ()
loop = do
    putStr "calc > "
    txt <- getLine
    print (lexer txt)
    loop

main :: IO ()
main = loop
