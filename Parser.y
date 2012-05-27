{
module Main (main) where

import Scanner
import System.IO
import System.Environment(getArgs)

import Data.Strict.Tuple
import Data.Either
import Data.Maybe
import Data.List
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    type        { Token (TType $$) _    }
    defid       { Token (TDefId $$) _   }
    id          { Token (TId $$) _      }
    int         { Token (TInt $$) _     }
    "::"        { Token TColons _       }
    "->"        { Token TArrow _        }
    '+'         { Token (TOp '+') _     }
    '-'         { Token (TOp '-') _     }
    '*'         { Token (TOp '*') _     }
    '('         { Token (TOp '(') _     }
    ')'         { Token (TOp ')') _     }
    '='         { Token (TOp '=') _     }
    '<'         { Token (TOp '<') _     }
    '>'         { Token (TOp '>') _     }
    '|'         { Token (TOp '|') _     }
    endl        { Token TEndL _         }

%right "->"
%nonassoc '<' '>'
%left '+' '-'
%left '*'

%%

Start
    : Program { $1 }

Program
    : Function                  { [$1] }
    | Program Function     { $2 : $1 }

Function
    : Declaration Realization Empty { Function $1 (reverse $2) }

Declaration
    : defid "::" Type endl            { Decl $1 $3 }

Type
    : Type "->" Type    { Arrow $1 $3 }
    | '(' Type ')'      { $2 }
    | type              { Simple $1 }

--AtomicType
--    : '(' Type ')'              { reverseType $2 }
--    | type                      { TypeT $1 }

Realization
    : Realization Pattern          { $2 : $1 }
    | Pattern                      { [$1] }

Pattern
    : defid VarList Condition '=' Expression endl  { Pattern $1 (reverse $2) $3 $5 }

VarList
    : {- empty -}   { [] }
    | VarList Var   { $2 : $1 }

Var : id    { Name $1 }
    | int   { Const $1 }

Condition
    : {- empty -}       { Nothing }
    | '|' Expression    { Just $2 }

Expression
    : id AtomicExpressionList   { Func $1 (reverse $2) }
    | Expression '<' Expression { Lt $1 $3 }
    | Expression '>' Expression { Gt $1 $3 }
    | Expression '+' Expression { Plus $1 $3 }
    | Expression '-' Expression { Minus $1 $3 }
    | Expression '*' Expression { Mul $1 $3 }
    | AtomicExpression          { $1 }

AtomicExpressionList
    : AtomicExpression                      { [$1] }
    | AtomicExpressionList AtomicExpression { $2 : $1 }

AtomicExpression
    : '(' Expression ')'    { $2 }
    | Var                   { VarExpr $1 }

Empty
    : endl          {}
    | Empty endl   {}


{
parseError :: [Token] -> a
parseError ((Token t (line :!: column)) : _) = error $ "unexpected token " ++ (show t) ++ " at position " ++ (show line) ++ ":" ++ (show column)

data Var = Name String | Const Int
    deriving Eq

instance Show Var where
    show = varToString

varToString :: Var -> String
varToString (Name s) = s
varToString (Const i) = show i

data Type = Arrow Type Type | Simple String
    deriving Eq

instance Show Type where
    show = typeToStr


typeToStr :: Type -> String
typeToStr (Arrow t1 t2) = "Function<" ++ (typeToStr t1) ++ ", " ++ (typeToStr t2) ++ ">"
typeToStr (Simple s) = s

--typeToList :: Type -> [Type]
--typeToList (Arr)

--instance Show Type where
--    show = typeToStr

--typeToStr :: Type -> String
--typeToStr (TypeT s) = s
--typeToStr (TypeL (x:[])) = typeToStr x
--typeToStr (TypeL (x:xs)) = "Function<" ++ (typeToStr x) ++ ", " ++ (typeToStr $ TypeL xs) ++ ">"

--unType :: Type -> [Type]
--unType t@(TypeT _) = [t]
--unType (TypeL l) = l

--reverseType :: Type -> Type
--reverseType t@(TypeT _) = t
--reverseType (TypeL l) = TypeL . reverse $ l

data Expr = 
    Lt Expr Expr |
    Gt Expr Expr |
    Plus Expr Expr |
    Minus Expr Expr |
    Mul Expr Expr |
    Func String [Expr] |
    VarExpr Var
    deriving (Eq, Show)

type Condition = Maybe Expr

exprToStr :: Expr -> [Var] -> String
exprToStr (Lt e1 e2) l              = (exprToStr e1 l) ++ " < " ++ (exprToStr e2 l)
exprToStr (Gt e1 e2) l              = (exprToStr e1 l) ++ " > " ++ (exprToStr e2 l)
exprToStr (Plus e1 e2) l            = (exprToStr e1 l) ++ " + " ++ (exprToStr e2 l)
exprToStr (Minus e1 e2) l           = (exprToStr e1 l) ++ " - " ++ (exprToStr e2 l)
exprToStr (Mul e1 e2) l             = (exprToStr e1 l) ++ " * " ++ (exprToStr e2 l)
exprToStr (Func f (x:xs)) l         = f ++ "(" ++ (exprToStr x l) ++ (concatMap (\v -> ", " ++ (exprToStr v l)) xs) ++ ")"
exprToStr (VarExpr v@(Name s)) l    = case elemIndex v l of
    Nothing -> s ++ "()"
    Just i  -> "param" ++ (show i)
exprToStr (VarExpr v@(Const i)) _   = show v

data Pattern = Pattern  { func :: String
                        , varList :: [Var]
                        , condition :: Maybe Expr
                        , expr :: Expr
                        } deriving Eq

instance Show Pattern where
    show = patternToStr

patternToStr :: Pattern -> String
patternToStr (Pattern _ l c e) = case fullCondition l c of
    Nothing -> returnStr
    Just s  -> "if (" ++ s ++ ") {\n" ++ returnStr ++ "\n}"
    where
        returnStr = "return " ++ (exprToStr e l) ++ ";"

fullCondition :: [Var] -> Maybe Expr -> Maybe String
fullCondition l Nothing = varCondition l
fullCondition l (Just e) = case varCondition l of
    Nothing -> Just cStr
    Just s  -> Just $ s ++ " && (" ++ cStr ++ ")"
    where
        cStr = exprToStr e l

varCondition :: [Var] -> Maybe String
varCondition l = case (filter (\(_, v) -> isConst v) (zip [0, 1 ..] l)) of
    []      -> Nothing
    x:xs    -> Just $ (showPair x) ++ (concatMap (\v -> " && " ++ (showPair v)) xs) where
        showPair (i, v) = "param" ++ (show i) ++ ".equals(" ++ (show v) ++ ")"

isConst :: Var -> Bool
isConst (Const _) = True
isConst _ = False

data Decl = Decl    { funcName :: String
                    , funcType :: Type
                    } deriving Eq

instance Show Decl where
    show = declToStr

declToStr :: Decl -> String
declToStr (Decl n t) = "public static " ++ (show l) ++ " " ++ n ++ "(" ++ (printTypeList (zip [0, 1..] i)) ++ ")"
    where
        printTypeList tlist = case tlist of
            []      -> ""
            x:[]    -> showPair x
            x:xs    -> (showPair x) ++ ", " ++ (printTypeList xs)
        l = last tl
        i = init tl
        tl = typeToList t
        showPair (i, v) = (show v) ++ " param" ++ (show i)
                

typeToList :: Type -> [Type]
typeToList (Arrow t1 t2) = t1 : (typeToList t2)
typeToList a@(Simple t) = [a]

data Function = Function Decl [Pattern] deriving Eq


instance Show Function where
    show = funcToStr

funcToStr :: Function -> String
funcToStr (Function d pl) = (show d) ++ " {\n" ++ (unlines resultRealization) ++ "}"
    where
        resultRealization = if (take 6 (last showPl)) /= "return" then (showPl ++ ["throw new IllegalArgumentException();"]) else showPl
        showPl = map show pl

--printType :: Type -> (String, String)
--printType 

--printType' :: Type -> String -> (String, String)
--printType' (Arrow t1 t2) s = printType' 

main = do
    args    <- getArgs
    input   <- readFile . head $ args
    writeFile "output" input
--    s <- getContents
--    print . parse . alexScanTokens $ s

}
