parser grammar MikoParserRules;

options {
     tokenVocab=MikoLexerRules;
     language=Cpp;
}

prog :
     ;

statement : ';'
          ;

define : defineHead (',' defineHead)? ;

defineHead : defineKeyword ID ':' type ;

defineKeyword : VAR
              | CONST
              | DEFINE
              ;

type : ;

callIdentifier : ID ('.' ID)* ;

callFunction : ID '(' functionArgs ')' ;

functionArgs : expression (',' expression)*
             ;

call : callFunction
     | callIdentifier
     ;
atomExpression : call
               | INT
               | FLOAT
               | CHAR
               | STRING
               ;

expression : '(' expression ')'                           # parent
           | expression '.' call                          # dot
           | '-' expression                               # minus
           | '~' expression                               # negate
           | '!' expression                               # not
           | '++' expression                              # increment
           | '--' expression                              # decrment  
           | expression ('*'|'/'|'%') expression          # multi
           | expression ('+'|'-') expression              # add
           | expression ('<<'|'>>') expression            # bitshift
           | expression ('>'|'>='|'<'|'<=') expression    # logicBoS
           | expression ('=='|'!=') expression            # logicEq
           | expression '&' expression                    # bitAnd
           | expression '^' expression                    # bitXor
           | expression '|' expression                    # bitOr
           | expression '&&' expression                   # logicAnd
           | expression '||' expression                   # logicOr
           | callIdentifier assignmentOperator expression # assign
           | openExpression                               # open
           | lambdaExpression                             # lambda
           | atomExpression                               # atom
           ;

openExpression : OPEN ID ('.' ID)? ;

assignmentOperator : '='
           | '/='
           | '*='
           | '%='
           | '+='
           | '-='
           | '<<='
           | '>>='
           | '&='
           | '^='
           | '|='
           ;

lambdaExpression : lambdaHead '.' lambdaBody ;

lambdaHead : '\\' '(' define ')'
           | '\\' defineHead;

lambdaBody : codeBlock
           | statement
           | expression
           ;

codeBlock :
          ;