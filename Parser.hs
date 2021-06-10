module Parser ( shuntingYard ) where

import Lexer ( Token(..) )

shuntingYard :: [Token] -> [Token]
shuntingYard ts = pf ++ op
    where (op, pf) = syHelper [] [] ts

syHelper :: [Token] -> [Token] -> [Token] -> ([Token], [Token])
syHelper op pf [] = (op, pf)
syHelper op pf (Number n:ts) = syHelper op (pf ++ [Number n]) ts
syHelper op pf (LParen:ts) = syHelper (LParen:op) pf ts
syHelper op pf (RParen:ts) = syHelper afterParen (pf ++ beforeParen) ts
    where
        beforeParen = takeWhile (/=LParen) op
        afterParen = tail (dropWhile (/=LParen) op)
syHelper op pf (t:ts) =
    case t of
        Plus -> syHelper (Plus:afterPM) (pf ++ beforePM) ts
        Minus -> syHelper (Minus:afterPM) (pf ++ beforePM) ts
        Multiply -> syHelper (Multiply:afterMDM) (pf ++ beforeMDM) ts
        Divide -> syHelper (Divide:afterMDM) (pf ++ beforeMDM) ts
        Modulo -> syHelper (Modulo:afterMDM) (pf ++ beforeMDM) ts
        Power -> syHelper (Power:afterP) (pf ++ beforeP) ts
        _ -> error "bad parse"
    where
        pmCond t = t `elem` [Multiply, Divide, Modulo, Power]
        mdmCond t = t == Power
        pCond t = t `elem` []
        beforePM = takeWhile pmCond op
        afterPM = dropWhile pmCond op
        beforeMDM = takeWhile mdmCond op
        afterMDM = dropWhile mdmCond op
        beforeP = takeWhile pCond op
        afterP = dropWhile pCond op
