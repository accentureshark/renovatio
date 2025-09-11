grammar Nql;

@header {
package org.shark.renovatio.shared.nql.antlr;
}

query
    : action target whereClause? inClause? returnClause? EOF
    ;

action
    : FIND
    | PLAN
    | APPLY
    ;

target
    : IDENTIFIER+
    ;

whereClause
    : WHERE predicate
    ;

predicate
    : PREDICATE_TEXT
    ;

inClause
    : IN scope
    ;

scope
    : SCOPE_TEXT
    ;

returnClause
    : RETURN returnExpr
    ;

returnExpr
    : RETURN_TEXT
    ;

FIND    : 'FIND';
PLAN    : 'PLAN';
APPLY   : 'APPLY';
WHERE   : 'WHERE' -> pushMode(PREDICATE_MODE);
IN      : 'IN' -> pushMode(SCOPE_MODE);
RETURN  : 'RETURN' -> pushMode(RETURN_MODE);
IDENTIFIER : [a-zA-Z_][a-zA-Z0-9_]*;
WS      : [ \t\r\n]+ -> skip;

mode PREDICATE_MODE;
PREDICATE_IN     : 'IN' -> type(IN), popMode, pushMode(SCOPE_MODE);
PREDICATE_RETURN : 'RETURN' -> type(RETURN), popMode, pushMode(RETURN_MODE);
PREDICATE_TEXT   : ~[\r\n]+ -> type(PREDICATE_TEXT);

mode SCOPE_MODE;
SCOPE_RETURN : 'RETURN' -> type(RETURN), popMode, pushMode(RETURN_MODE);
SCOPE_TEXT   : ~[\r\n]+ -> type(SCOPE_TEXT);

mode RETURN_MODE;
RETURN_TEXT  : ~[\r\n]+ -> type(RETURN_TEXT);

