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
WHERE   : 'WHERE';
IN      : 'IN';
RETURN  : 'RETURN';
IDENTIFIER : [a-zA-Z_][a-zA-Z0-9_]*;
PREDICATE_TEXT : ~[\r\n]+ ;
SCOPE_TEXT     : ~[\r\n]+ ;
RETURN_TEXT    : ~[\r\n]+ ;
WS      : [ \t\r\n]+ -> skip;
