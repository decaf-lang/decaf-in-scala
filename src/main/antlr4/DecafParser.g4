parser grammar DecafParser;

options { tokenVocab=DecafLexer; }

// Classes and fields

topLevel
    : classDef*
    ;

classDef
    : CLASS id extendsClause? '{' field* '}'
    ;

extendsClause
    : EXTENDS id
    ;

field
    : varDef
    | methodDef
    ;

varDef
    : var ';'
    ;

methodDef
    : STATIC? type id '(' varList ')' stmtBlock
    ;

var
    : type id
    ;

varList
    : var (',' var)*
    | /* empty */
    ;

// Types

type
    : INT                       # intType
    | BOOL                      # boolType
    | STRING                    # stringType
    | VOID                      # voidType
    | CLASS id                  # classType
    | elemType=type '[' ']'     # arrayType
    ;

// Statements

stmt
    : varDef                                                                # localVarDef
    | stmtBlock                                                             # block
    | simple ';'                                                            # simpleStmt
    | IF '(' cond=expr ')' trueBranch=stmt (ELSE falseBranch=stmt)?         # if
    | WHILE '(' cond=expr ')' body=stmt                                     # while
    | FOR '(' init=simple ';' cond=expr ';' update=simple ')' body=stmt     # for
    | BREAK ';'                                                             # break
    | RETURN expr? ';'                                                      # return
    | PRINT '(' exprList ')' ';'                                            # print
    ;

stmtBlock
    : '{' stmt* '}'
    ;

simple
    : lValue '=' expr                       # assign
    | (expr '.')? id '(' exprList ')'       # eval
    | /* empty */                           # skip
    ;

lValue
    : (expr '.')? id                        # lValueVar
    | array=expr '[' index=expr ']'         # lValueIndex
    ;

// Expressions

expr
    : lit                                           # literal
    | THIS                                          # this
    | '(' expr ')'                                  # paren
    | '(' CLASS id ')' expr                         # classCast
    | READ_INTEGER '(' ')'                          # readInt
    | READ_LINE '(' ')'                             # readLine
    | NEW id '(' ')'                                # newClass
    | NEW elemType=type '[' length=expr ']'         # newArray
    | INSTANCEOF '(' expr ',' id ')'                # classTest
    | varSelOrCall                                  # singlePath
    | expr '.' varSelOrCall                         # path
    | array=expr '[' index=expr ']'                 # indexSel
    | prefix=('-'|'!') expr                         # unary
    | lhs=expr infix=('*'|'/'|'%') rhs=expr         # binary
    | lhs=expr infix=('+'|'-') rhs=expr             # binary
    | lhs=expr infix=('<='|'<'|'>='|'>') rhs=expr   # binary
    | lhs=expr infix=('=='|'!=') rhs=expr           # binary
    | lhs=expr infix='&&' rhs=expr                  # binary
    | lhs=expr infix='||' rhs=expr                  # binary
    ;

lit
    : INT_LIT         # intLit
    | BOOL_LIT        # boolLit
    | NULL_LIT        # nullLit
    | OPEN_STRING stringChar* (CLOSE_STRING | UNTERM_STRING)    # stringLit
    ;

stringChar
    : ERROR_NEWLINE
    | VALID_CHAR;

varSelOrCall
    : id ('(' exprList ')')?
    ;

exprList
    : expr (',' expr)*
    | /* empty */
    ;

id  : ID ;