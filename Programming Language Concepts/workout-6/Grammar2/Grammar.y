{
module Grammar where
import Tokens
import Prog
}

%name parseProg
%tokentype { Token }
%error { parseError }

%token
    '('  { TokenLParen }
    ')'  { TokenRParen }
    ';'  { TokenSemi}
    ','  { TokenComma}
    id  { TokenId $$ }
%%

-- replace with your productions:
Prog
: id '(' Prog ')' ';' Prog      { [FunCall $1 $3] ++ $6 }
| id '(' Prog ')' ',' Prog      { [FunCall $1 $3] ++ $6 }
| id ',' Prog                   { (Var $1) : $3 }
| id ';' Prog                   { (Var $1) : $3 }
| id '(' ')' ';' Prog           { [FunCall $1 []] ++ $5 }
| id '(' ')'                    { [FunCall $1 []] }
| id                            { [Var $1] }
| {- empty -}                   { [] }



{

parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)

}
