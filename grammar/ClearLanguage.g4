grammar ClearLanguage;

start: funcDecl+ EOF;

expr: addExpr;

addExpr
    : left=mulExpr (op=('+'|'-') right=mulExpr)*
    ;

mulExpr
    : left=unaryExpr (op=('*'|'/') right=unaryExpr)*
    ;

unaryExpr
    : '-' inner=unaryExpr        #unaryMinus
    | primary                    #unaryPrimary
    ;

primary
    : INT                        #intLiteral
    | IDENT                      #varRef
    | '(' expr ')'               #parenExpr
    ;

attributes
    : '[' IDENT (',' IDENT)* ']'
    ;

funcDecl
    : attributes* FUNC IDENT '(' paramList? ')' returnType? block
    ;

paramList
    : IDENT (',' IDENT)*
    ;

returnType
    : ARROW type
    ;

type
    : IDENT
    ;

block
    : '{' stmt* '}'
    ;

stmt
    : varDecl          #stmtVarDecl
    | expr SEMI        #stmtExpr
    ;

varDecl
    : IDENT COLON type (ASSIGN expr)? SEMI
    ;

FUNC: 'func';
ARROW: '->';
SEMI: ';';
COLON: ':';
ASSIGN: '=';
IDENT: [a-zA-Z_][a-zA-Z0-9_]*;

INT: [0-9]+;

LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
WS: [ \t\r\n]+ -> skip;