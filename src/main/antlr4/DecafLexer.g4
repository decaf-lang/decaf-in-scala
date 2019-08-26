lexer grammar DecafLexer;

// Keywords

BOOL:           'bool';
BREAK:          'break';
CLASS:          'class';
ELSE:           'else';
EXTENDS:        'extends';
FALSE:          'false';
FOR:            'for';
IF:             'if';
INSTANCEOF:     'instanceof';
INT:            'int';
NEW:            'new';
NULL:           'null';
RETURN:         'return';
PRINT:          'print';
READ_INTEGER:   'ReadInteger';
READ_LINE:      'ReadLine';
STATIC:         'static';
STRING:         'string';
THIS:           'this';
TRUE:           'true';
VOID:           'void';
WHILE:          'while';

// Operators

ASSIGN:         '=';
NOT:            '!';
ADD:            '+';
SUB:            '-';
MUL:            '*';
DIV:            '/';
MOD:            '%';
AND:            '&&';
OR:             '||';
EQ:             '==';
NE:             '!=';
LE:             '<=';
LT:             '<';
GE:             '>=';
GT:             '>';

// Separators

DOT:            '.';
COMMA:          ',';
SEMI:           ';';
LPAREN:         '(';
RPAREN:         ')';
LBRACK:         '[';
RBRACK:         ']';
LBRACE:         '{';
RBRACE:         '}';

// Identifiers

ID:             [A-Za-z][_0-9A-Za-z]*;

// Literals

INT_LIT:        [0-9]+ | [0][Xx][0-9A-Fa-f]+;
BOOL_LIT:       TRUE | FALSE;
NULL_LIT:       NULL;

STRING_LIT:         '"' ~[\r\n"]* '"';
NEWLINE_STRING_LIT: '"' ~[\r\n"]* [\r\n];
UNTERM_STRING_LIT:  '"' ~[\r\n"]*;

// Whitespace and comments

WHITESPACE:     [ \t\r\n]+ -> channel(HIDDEN);
COMMENT:        '//' ~[\r\n]* -> channel(HIDDEN);

// Error

UNRECOG_Char:   .;