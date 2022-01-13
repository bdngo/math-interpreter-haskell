module Interpreter (
    interpretTree,
    interpretStack
) where

import Data.Function ( on )
import Lexer ( Token(..) )
import Parser ( Node(..), Operator(..) )

interpretTree :: Node -> Double
interpretTree (Number n) = n
interpretTree (Prim2 Plus e1 e2) = ((+) `on` interpretTree) e1 e2
interpretTree (Prim2 Minus e1 e2) = ((-) `on` interpretTree) e1 e2
interpretTree (Prim2 Multiply e1 e2) = ((*) `on` interpretTree) e1 e2
interpretTree (Prim2 Divide e1 e2) = ((/) `on` interpretTree) e1 e2
interpretTree (Prim2 Modulo e1 e2) = fromIntegral $ (mod `on` (round . interpretTree)) e1 e2
interpretTree (Prim2 Power e1 e2) = ((**) `on` interpretTree) e1 e2

interpretStack :: [Token] -> Double
interpretStack toks =
    case head $ foldl foldFunc [] toks of
        NUMBER n -> n
        _        -> error "bad expression"

foldFunc :: [Token] -> Token -> [Token]
foldFunc [] t = [t]
foldFunc [x] t = t:[x]
foldFunc (NUMBER n1:NUMBER n2:ts) t =
    case t of
        NUMBER n -> NUMBER n:NUMBER n1:NUMBER n2:ts
        PLUS     -> NUMBER (n1 + n2):ts
        MINUS    -> NUMBER (n2 - n1):ts
        MULTIPLY -> NUMBER (n1 * n2):ts
        DIVIDE   -> NUMBER (n2 / n1):ts
        MODULO   -> NUMBER (fromIntegral ((mod `on` round) n2 n1)):ts
        POWER    -> NUMBER (n2 ** n1):ts
        _        -> error "bad expression"
foldFunc (NUMBER _:_:_) _ = error "bad expression"
foldFunc (_:_:_) _ = error "bad expression"
