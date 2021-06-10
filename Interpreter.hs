{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Interpreter (
    interpret
) where

import Data.Function ( on )
import Parser ( shuntingYard )
import Lexer (
        Token (
            Plus,
            Minus,
            Multiply,
            Divide,
            Modulo,
            Power,
            LParen,
            RParen,
            Number
        ),
        Operator,
        isOperator
    )

interpret :: (Operator a) => [a] -> Double
interpret [] = error "empty postfix"
interpret [Number n] = n
interpret [_] = error "bad postfix"
interpret (t1:t2:xs)
