grammar ClearLanguage;

start: packageDecl? importDecl* (funcDecl)* EOF;

packageDecl
    : PACKAGE qualifiedIdent SEMI
    ;

importDecl
    : IMPORT qualifiedIdent (AS IDENT)? SEMI
    ;

qualifiedIdent
    : IDENT ('::' IDENT)*
    ;

expr: orExpr;

orExpr
    : left=andExpr (OR right+=andExpr)*
    ;

andExpr
    : left=equalExpr (AND right+=equalExpr)*
    ;

equalExpr
    : left=addExpr (op+=(EQ|NEQ) right+=addExpr)*
    ;

addExpr
    : left=mulExpr (op+=(PLUS|MINUS) right+=mulExpr)*
    ;

PLUS: '+';
MINUS: '-';

mulExpr
    : left=unaryExpr (op+=('*'|'/'|'%') right+=unaryExpr)*
    ;

unaryExpr
    : '-' inner=unaryExpr        #unaryMinus
    | postfixExpr                    #unaryPrimary
    ;

postfixExpr
    : primary (callSuffix | asSuffix | asForceSuffix)*
    ;

asSuffix
    : AS type
    ;

asForceSuffix
    : ASForce type
    ;

callSuffix
    : '(' argList? ')'
    ;

primary
    : FLOAT                      #floatLiteral
    | INT                        #intLiteral
    | STRING                     #stringLiteral
    | (TRUE | FALSE)             #boolLiteral
    | IDENT                      #varRef
    | '(' expr ')'               #parenExpr
    | '(' ')'                    #unitLiteral
    ;

attributes
    : '[' IDENT (',' IDENT)* ']'
    ;

funcDecl
    : attributes* FUNC name=IDENT '(' paramList? ')' ARROW type block
    ;

paramList
    : param (',' param)*
    ;

param
    : IDENT ':' type
    ;

type
    : IDENT #namedType
    | '(' typeList? ')' ARROW type #functionType
    | '(' ')' #unitType
    ;

typeList
    : type (',' type)*
    ;

block
    : '{' stmt* '}'
    ;

stmt
    : RETURN expr? SEMI  #stmtReturn
    | varDecl            #stmtVarDecl
    | ifStmt             #stmtIf
    | expr SEMI          #stmtExpr
    ;

ifStmt
    : IF '(' expr ')' block (ELSE (block | stmt))? #ifBlock
    | IF '(' expr ')' stmt (ELSE (block | stmt))? #ifSingle
    ;

varDecl
    : IDENT COLON type (ASSIGN expr)? SEMI
    ;

argList
    : expr (',' expr)*
    ;

PACKAGE: 'package';
IMPORT: 'import';
IF: 'if';
ELSE: 'else';
FUNC: 'func';
ARROW: '->';
SEMI: ';';
COLON: ':';
TRUE: 'true';
FALSE: 'false';
EQ: 'is';
NEQ: 'not';
AND: 'and';
OR: 'or';
ASSIGN: '=';
RETURN: 'return';


STRING: '"' (ESC_SEQ | ~["\\])* '"';
fragment ESC_SEQ: '\\' [btnfr"'\\];
ASForce: 'as!';
AS: 'as';
IDENT: [a-zA-Z_][a-zA-Z0-9_]*;
fragment DIGITS: [0-9]+;
fragment EXP: [eE] [+-]? DIGITS;
FLOAT: DIGITS '.' DIGITS? EXP? | '.' DIGITS EXP? | DIGITS EXP;
INT: [0-9]+;

LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
WS: [ \t\r\n]+ -> skip;