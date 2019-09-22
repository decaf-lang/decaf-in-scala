lexer grammar DecafLexer;

// Keywords (except those literals)

BOOL:           'bool';
BREAK:          'break';
CLASS:          'class';
ELSE:           'else';
EXTENDS:        'extends';
FOR:            'for';
IF:             'if';
INSTANCEOF:     'instanceof';
INT:            'int';
NEW:            'new';
RETURN:         'return';
PRINT:          'Print';
READ_INTEGER:   'ReadInteger';
READ_LINE:      'ReadLine';
STATIC:         'static';
STRING:         'string';
THIS:           'this';
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

// Literals

INT_LIT:            [0-9]+ | [0][Xx][0-9A-Fa-f]+;
BOOL_LIT:           'true' | 'false';
NULL_LIT:           'null';

// For strings: we only handle characters that consists of a string in the lexer grammar,
// and leave the concatenation for parser grammar.

OPEN_STRING:        '"' -> pushMode(IN_STRING);

// Whitespace and comments

WHITESPACE:         [ \t\r\n]+ -> skip;
COMMENT:            '//' ~[\r\n]* -> skip;

// Identifiers

ID:                 [A-Za-z][_0-9A-Za-z]*;

// Error

UNRECOG_Char:       .;

// ------ When inside a string literal ------

mode IN_STRING;

ERROR_NEWLINE:      '\r\n' | '\r' | '\n';
ESC:                '\\' [nrt"\\];
BAD_ESC:            '\\' ~[nrt"\\];
VALID_CHAR:         ~[\r\n"];

CLOSE_STRING:       '"' -> popMode;
UNTERM_STRING:      EOF -> popMode;
