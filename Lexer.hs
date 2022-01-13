module Lexer (
    Token(..),
    lexer
) where

import Data.Char ( isDigit, isSpace )
import Control.Exception
    ( throw, PatternMatchFail(PatternMatchFail) )

data Token = PLUS
    | MINUS
    | MULTIPLY
    | DIVIDE
    | MODULO
    | POWER
    | LPAREN
    | RPAREN
    | NUMBER Double
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer ('+':xs) = PLUS     : lexer xs
lexer ('-':xs) = MINUS    : lexer xs
lexer ('*':xs) = MULTIPLY : lexer xs
lexer ('/':xs) = DIVIDE   : lexer xs
lexer ('%':xs) = MODULO   : lexer xs
lexer ('^':xs) = POWER    : lexer xs
lexer ('(':xs) = LPAREN   : lexer xs
lexer (')':xs) = RPAREN   : lexer xs
lexer (x:xs)
    | isSpace x = lexer xs
    | x == '.' || isDigit x = numToken : lexer rest
    | otherwise = throw (PatternMatchFail ("illegal character: " ++ [x]))
    where (numToken, rest) = tokenizeNum [x] xs

tokenizeNum :: String  -> String -> (Token, String)
tokenizeNum s [] = (NUMBER (read (addZero s)), [])
tokenizeNum s (x:xs)
    | x == '.' && '.' `elem` s = (NUMBER (read (addZero s)), xs)
    | isDigit x || (x == '.' && '.' `notElem` s) = tokenizeNum (s ++ [x]) xs
    | otherwise = (NUMBER (read (addZero s)), x:xs)

addZero :: String -> String
addZero ('.':xs) = '0':'.':xs
addZero xs = if last xs == '.' then xs ++ "0" else xs
