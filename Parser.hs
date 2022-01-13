module Parser ( parse, shuntingYard, Node(..), Operator(..) ) where

import Lexer ( Token(..) )

data Operator = Plus | Minus | Multiply | Divide | Modulo | Power
    deriving (Show, Eq)

data Node = Prim2 Operator Node Node | Number Double
    deriving (Show, Eq)

{-
<expr> ::=
    | <term> <expr'>

<expr'> ::=
    | + <expr>
    | - <expr>
    | e

<term> ::=
    | <exponent> <term'>

<term'> ::=
    | * <term>
    | / <term>
    | % <term>
    | e

<exponent> ::=
    | <factor> <exponent'>

<exponent'> ::=
    | ^ <exponent>
    | e

<factor> ::=
    | NUM n
    | (<expr>)
-}

parse :: [Token] -> Node
parse ts =
    case parseExpr ts of
        (e, []) -> e
        _ -> error "bad parse"

parseExpr :: [Token] -> (Node, [Token])
parseExpr toks =
    let (term, termRest) = parseTerm toks in
    case termRest of
        PLUS : exprRest ->
            let (exprRight, exprRightRest) = parseExpr exprRest in
            (Prim2 Plus term exprRight, exprRightRest)
        MINUS : exprRest ->
            let (exprRight, exprRightRest) = parseExpr exprRest in
            (Prim2 Minus term exprRight, exprRightRest)
        _ -> (term, termRest)

parseTerm :: [Token] -> (Node, [Token])
parseTerm toks =
    let (exponent, exponentRest) = parseExponent toks in
    case exponentRest of
        MULTIPLY : termRest ->
            let (termRight, termRightRest) = parseTerm termRest in
            (Prim2 Multiply exponent termRight, termRightRest)
        DIVIDE : termRest ->
            let (termRight, termRightRest) = parseTerm termRest in
            (Prim2 Divide exponent termRight, termRightRest)
        MODULO : termRest ->
            let (termRight, termRightRest) = parseExpr termRest in
            (Prim2 Modulo exponent termRight, termRightRest)
        _ -> (exponent, exponentRest)

parseExponent :: [Token] -> (Node, [Token])
parseExponent toks =
    let (factor, factorRest) = parseFactor toks in
    case factorRest of
        POWER : termRest ->
            let (exponentRight, exponentRightRest) = parseExpr termRest in
            (Prim2 Power factor exponentRight, exponentRightRest)
        _ -> (factor, factorRest)

parseFactor :: [Token] -> (Node, [Token])
parseFactor (NUMBER n : rest) = (Number n, rest)
parseFactor (LPAREN : rest) =
    let (expr, paren:parenRest) = parseExpr rest in
    if paren == RPAREN then (expr, parenRest) else error "bad parse"
parseFactor _ = error "bad parse"

shuntingYard :: [Token] -> [Token]
shuntingYard ts = pf ++ op
    where (op, pf) = syHelper [] [] ts

syHelper :: [Token] -> [Token] -> [Token] -> ([Token], [Token])
syHelper op pf [] = (op, pf)
syHelper op pf (NUMBER n:ts) = syHelper op (pf ++ [NUMBER n]) ts
syHelper op pf (LPAREN:ts) = syHelper (LPAREN:op) pf ts
syHelper op pf (RPAREN:ts) = syHelper afterParen (pf ++ beforeParen) ts
    where
        beforeParen = takeWhile (/=LPAREN ) op
        afterParen = tail (dropWhile (/=LPAREN ) op)
syHelper op pf (t:ts) =
    case t of
        PLUS -> syHelper (PLUS:afterPM) (pf ++ beforePM) ts
        MINUS -> syHelper (MINUS:afterPM) (pf ++ beforePM) ts
        MULTIPLY -> syHelper (MULTIPLY:afterMDM) (pf ++ beforeMDM) ts
        DIVIDE -> syHelper (DIVIDE:afterMDM) (pf ++ beforeMDM) ts
        MODULO -> syHelper (MODULO:afterMDM) (pf ++ beforeMDM) ts
        POWER -> syHelper (POWER:afterP) (pf ++ beforeP) ts
        _ -> error "bad parse"
    where
        pmCond t = t `elem` [MULTIPLY, DIVIDE, MODULO, POWER]
        mdmCond t = t == POWER
        pCond t = t `elem` []
        beforePM = takeWhile pmCond op
        afterPM = dropWhile pmCond op
        beforeMDM = takeWhile mdmCond op
        afterMDM = dropWhile mdmCond op
        beforeP = takeWhile pCond op
        afterP = dropWhile pCond op
