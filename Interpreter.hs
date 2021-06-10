module Interpreter (
    interpret
) where

import Data.Function ( on )
import Parser ( shuntingYard )
import Lexer ( Token (..) )

interpret :: [Token] -> Token
interpret = head . foldl foldFunc []

foldFunc :: [Token] -> Token -> [Token]
foldFunc [] t = [t]
foldFunc [x] t = t:[x]
foldFunc (Number n1:Number n2:ts) t =
    case t of
        Number n -> Number n:Number n1:Number n2:ts
        Plus     -> Number (n1 + n2):ts
        Minus    -> Number (n2 - n1):ts
        Multiply -> Number (n1 * n2):ts
        Divide   -> Number (n2 / n1):ts
        Modulo   -> Number (fromIntegral ((mod `on` round) n2 n1)):ts
        Power    -> Number (n2 ** n1):ts
        _        -> error "bad parse"
foldFunc (Number _:_:_) _ = error "bad parse"
foldFunc (_:_:_) _ = error "bad parse"
