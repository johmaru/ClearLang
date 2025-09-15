grammar ClearLanguage;

start: funcDecl+ EOF;

expr: addExpr;

addExpr
    : left=mulExpr (op+=('+'|'-') right+=mulExpr)*
    ;

mulExpr
    : left=unaryExpr (op+=('*'|'/') right+=unaryExpr)*
    ;

unaryExpr
    : '-' inner=unaryExpr        #unaryMinus
    | postfixExpr                    #unaryPrimary
    ;

postfixExpr
    : primary (callSuffix)*
    ;

callSuffix
    : '(' argList? ')'
    ;

primary
    : FLOAT                      #floatLiteral
    | INT                        #intLiteral
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
    | varDecl          #stmtVarDecl
    | expr SEMI        #stmtExpr
    ;

varDecl
    : IDENT COLON type (ASSIGN expr)? SEMI
    ;

argList
    : expr (',' expr)*
    ;

FUNC: 'func';
ARROW: '->';
SEMI: ';';
COLON: ':';
ASSIGN: '=';
RETURN: 'return';
IDENT: [a-zA-Z_][a-zA-Z0-9_]*;
fragment DIGITS: [0-9]+;
fragment EXP: [eE] [+-]? DIGITS;
FLOAT: DIGITS '.' DIGITS? EXP? | '.' DIGITS EXP? | DIGITS EXP;
INT: [0-9]+;

LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
WS: [ \t\r\n]+ -> skip;