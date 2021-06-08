module Lexer (
    Token,
    lexer
) where

import Data.Char ( isDigit, isSpace )

data Token = Plus | Minus | Multiply | Divide | LParen | RParen | Number Double
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer ('+':xs) = Plus : lexer xs
lexer ('-':xs) = Minus : lexer xs
lexer ('*':xs) = Multiply : lexer xs
lexer ('/':xs) = Divide : lexer xs
lexer ('(':xs) = LParen : lexer xs
lexer (')':xs) = RParen : lexer xs
lexer (x:xs)
    | isSpace x = lexer xs
    | x == '.' || isDigit x = numToken : lexer rest
    | otherwise = error ("illegal character: " ++ [x])
    where (numToken, rest) = tokenizeNum [x] xs

tokenizeNum :: String  -> String -> (Token, String)
tokenizeNum s [] = (Number (read (addZero s)), [])
tokenizeNum s (x:xs)
    | x == '.' && '.' `elem` s = (Number (read (addZero s)), xs)
    | isDigit x || (x == '.' && '.' `notElem` s) = tokenizeNum (s ++ [x]) xs
    | otherwise = (Number (read (addZero s)), x:xs)

addZero :: String -> String
addZero ('.':xs) = '0':'.':xs
addZero xs = if last xs == '.' then xs ++ "0" else xs