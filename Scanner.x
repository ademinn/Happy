{
module Scanner where

import Data.Strict.Tuple
}

%wrapper "posn"

$digit      = [0-9]
$space      = [\ \t]
$line       = [\f\v\r\n]

@idTail     = [A-Za-z0-9_]*

tokens :-

    $space+             ;
    $line               { makeToken (\_ -> TEndL) }
    $digit+             { makeToken (\s -> TInt (read s)) }
    [A-Z] @idTail       { makeToken (\s -> TType s) }
    ^ [a-z_] @idTail    { makeToken (\s -> TDefId s) }
    [a-z_] @idTail      { makeToken (\s -> TId s) }
    "->"                { makeToken (\_ -> TArrow) }
    "::"                { makeToken (\_ -> TColons) }
    [\=\+\-\*\/\(\)\|\<\>]  { makeToken (\s -> TOp (head s)) }

{
data Token = Token TokenType (Pair Int Int)
    deriving (Eq, Show)

getPos :: AlexPosn -> Pair Int Int
getPos (AlexPn _ line column) = line :!: column

makeToken :: (String -> TokenType) -> AlexPosn -> String -> Token
makeToken f p s = Token (f s) (getPos p)

data TokenType =
    TType    String  |
    TDefId   String  |
    TId      String  |
    TInt     Int     |
    TOp      Char    |
    TArrow           |
    TColons          |
    TEndL
    deriving (Eq, Show)
}
