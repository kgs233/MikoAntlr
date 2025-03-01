parser grammar MikoParserRules;

options {
     tokenVocab=MikoLexerRules;
     language=Cpp;
}

prog :  (openStatement|structMember)*
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

forStatement : FOR '(' defineExpression (',' expression)* ';' expression ';' expression ')' (codeBlock|statement) ;

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

defineStatement : defineKeyword defineExpression (',' defineExpression)* ';' ;

defineExpression : ID ':' type ('=' expression)? ;

type : compilerCall
     | call
     | defineEnum
     | structType
     | defineEnum
     | lambdaExpression
     | '(' type ('|' type)* ')'
     | '(' type (',' type)* ')'
     ;

externCall : CALL ID                        # externVar
           | CALL ID '(' functionArgs ')'   # externFunc
           | CALL STRUCT '{' structMember* '}' # externStruct
           ;

structType : STRUCT extendObject? '{' (openStatement|structMember)* '}';

structMember : (accessKeyword)? (STATIC)? defineStatement ;

defineEnum : ENUM extendObject? '{' enumMember (',' enumMember)* '}' ;

enumMember : (ID|ID '=' expression)
           ;

extendObject : '(' call ')' ;

compilerCall : '@' ID                      # compilerVar
             | '@' ID '(' functionArgs ')' # compilerFunc
             ;

call : ID ('.' ID)*
     | ID '(' functionArgs ')'
     ;

functionArgs : expression (',' expression)*
             ;

atomExpression : call
               | INT
               | FLOAT
               | CHAR
               | STRING
               ;

expression : call                                         # object
           | '(' expression ')'                           # parent
           | ',' expression                               # comma
           | '.' expression                               # dot
           | '-' expression                               # minus
           | '~' expression                               # negate
           | '!' expression                               # not
           | expression '++'                              # increment
           | expression '--'                              # decrment  
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
           | expression assignmentOperator expression     # assign
           | THIS                                         # this
           | openExpression                               # open
           | lambdaExpression                             # lambda
           | atomExpression                               # atom
           ;

openExpression : OPEN ID ('.' ID)* ;

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

lambdaHead : LAMBDA '(' defineExpression (',' defineExpression)* ')' ;

lambdaBody : codeBlock
           | returncodeBlock
           | statement
           | expression
           ;

codeBlock : '{' statement (statement)* '}' ;

returncodeBlock : type codeBlock ;