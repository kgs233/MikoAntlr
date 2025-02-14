parser grammar MikoParserRules;

options {
     tokenVocab=MikoLexerRules;
     language=Cpp;
}

prog :  (structBody)*
     ;

statement : ';'
          | expression ';'
          | defineStatement ';'
          | ifStatement
          | matchStatement
          | forStatement
          | whileStatement
          | returnStatement
          | openStatement
          ;

ifStatement : IF '(' expression ')' (codeBlock|statement)
            | IF '(' expression ')' (codeBlock|statement) ELSE (codeBlock|statement)
            ;

matchStatement : MATCH '(' expression ')' '{' matchMember (matchMember)* '}' ELSE (codeBlock|statement) ;

matchMember : expression ':' (codeBlock|statement) ;

forStatement : FOR '(' define ';' expression ';' expression ')' (codeBlock|statement) ;

whileStatement : WHILE '(' expression ')' (codeBlock|statement) ;

returnStatement : RETURN expression? ';' ;

openStatement : OPEN expression ';' ;

accessKeyword : PUBLIC
              | PRIVATE
              | LOCAL
              ;

defineKeyword : VAR
              | CONST
              | DEFINE
              ;

defineStatement : defineKeyword define ';' ;

define : defineExpression ('=' expression)? (',' defineExpression ('=' expression)?)* ;

defineExpression : ID ':' type ;

type : call
     | call '[' expression? ']'
     | structType
     | defineEnum
     | lambdaExpression
     ;

structType : STRUCT ('(' call ')')? '{' structBody '}';

structBody : (openStatement|structDefineStatement) (openStatement|structDefineStatement)*
           ;

structDefineStatement : (accessKeyword)? (STATIC)? defineStatement ;

defineEnum : ENUM ('(' call ')')? (':' type)? '{' enumBody '}' ;

enumBody : ID (',' ID)*
         | ID '=' expression (ID '=' expression)*
         ;

call : compilerCall
     | programCall
     ;

compilerCall : '@' callFunction
             | '@' callIdentifier
             ;

programCall : callFunction
            | callIdentifier
            ;

callIdentifier : ID ('.' ID)* ;

callFunction : ID '(' functionArgs ')' ;

functionArgs : expression (',' expression)*
             ;

atomExpression : call
               | INT
               | FLOAT
               | CHAR
               | STRING
               ;

expression : '(' expression ')'                           # parent
           | expression ',' expression                    # comma
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
           | THIS                                         # this
           | openExpression                               # open
           | lambdaExpression                             # lambda
           | atomExpression                               # atom
           ;

openExpression : OPEN callIdentifier ;

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

lambdaHead : LAMBDA '(' define* ')' ;

lambdaBody : codeBlock
           | returncodeBlock
           | statement
           | expression
           ;

codeBlock : '{' statement (statement)* '}' ;

returncodeBlock : type codeBlock ;