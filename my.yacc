open DataTypes ; 
exception VariableRedeclarationException of string ; 
exception TypeMisMatchException ; 
exception UnDeclaredVariableException ;  

%%
%name My

%term LBRACE | RBRACE | LPAREN | RPAREN | EOF | 
      IF | THEN| ELSE| FI | WHILE | DO | OD | 
      COLON | SEMICOLON | COMMA | INT | BOOL | RAT | INVERSE | RATFUN |
      LT | LEQ | NEQ | EQ | GT | GEQ | 
      PLUS | MINUS | TIMES | DIV | MOD | 
      RPLUS | RMINUS | RTIMES | RDIV  |
      NOT | AND | OR | TT | FF | NEG | 
      ASSIGN | READ | WRITE | CALL | PRINT| 
      MAKERAT | FROMDECIMAL | POSNUMERAL of string | RATIONAL of string | IDENTIFIER of string | PROCEDURE

%nonterm start of AST | blk of BLK | 
         decseq of DECSEQ | comseq of CMD list  | vardec of VARDEC  |
         ratdec of RATDEC | intdec of INTDEC | booldec of BOOLDEC |
         commands of CMD list | procdec of PROCDEF list | procdef of PROCDEF | 
         command of CMD  |
         varlist of string list |
         expression of Exp


%eop EOF
%noshift EOF
%pos int 
%verbose 

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GT GEQ LT LEQ
%left PLUS MINUS 
%left TIMES MOD DIV 
%left RPLUS RMINUS 
%left RTIMES RDIV 
%right NOT NEG INVERSE
%left LPAREN RPAREN 

%%

start: blk (PROG(blk))
blk: decseq comseq (BLK(decseq , comseq ))
decseq:  vardec procdec  (DECSEQ(vardec,procdec))

vardec:  ratdec intdec booldec (VARDEC(ratdec, intdec, booldec))

comseq: LBRACE commands RBRACE ((commands))

commands: (([]))
        | command SEMICOLON commands ((command::commands))

ratdec: (RATDEC(Rational,[]))
        | RAT varlist SEMICOLON (( RATDEC(Rational, varlist)))
intdec: (INTDEC(Int, []))
        |INT varlist SEMICOLON (( INTDEC(Int, varlist)))
booldec: (BOOLDEC(Bool, []))
        |BOOL varlist SEMICOLON ((BOOLDEC(Bool, varlist)))

procdec : (([]))
        | procdef SEMICOLON procdec ((procdef::procdec))
procdef : PROCEDURE IDENTIFIER blk   (PROCDEF(IDENTIFIER,blk))

varlist:  IDENTIFIER (([IDENTIFIER]))
         | IDENTIFIER COMMA varlist  ((IDENTIFIER::varlist))

command: READ LPAREN IDENTIFIER RPAREN ((Read(IDENTIFIER)))
        | PRINT LPAREN expression RPAREN((Print(expression)))
        | IDENTIFIER ASSIGN expression ((SET(IDENTIFIER , expression )))
        | IF expression THEN comseq ELSE comseq FI  (ITE(  expression , comseq1  , comseq2))
        | WHILE expression DO comseq OD ( (WH(  expression , comseq )))
        | CALL IDENTIFIER ((Call(IDENTIFIER)))
expression: expression LEQ expression ( ( LEQ( expression1 ,  expression2)))
            | expression LT expression ( ( LT( expression1 ,  expression2)))
            | expression EQ expression (( EQ( expression1 ,  expression2)))
            | expression NEQ expression (( NEQ( expression1 ,  expression2)) )
            | expression GT expression (( GT( expression1 ,  expression2)))
            | expression GEQ expression (( GEQ( expression1 ,  expression2)))
            
            | expression PLUS expression ( (PLUS( expression1 ,  expression2)))
            | expression MINUS expression (( MINUS( expression1 ,  expression2)))
            | expression TIMES expression (( TIMES( expression1 ,  expression2)))
            | expression DIV expression (( DIV( expression1 ,  expression2)))
            | expression MOD expression (( MOD( expression1 ,  expression2)))

            | expression RPLUS expression (( RPLUS( expression1 ,  expression2)))
            | expression RMINUS expression (( RMINUS( expression1 ,  expression2)))
            | expression RTIMES expression (( RTIMES( expression1 ,  expression2)))
            | expression RDIV expression (( RDIV( expression1 ,  expression2)))
           
            | expression AND expression ((AND( expression1 ,  expression2))  )
            | expression OR expression ((OR( expression1 ,  expression2)) )
        
            | LPAREN expression RPAREN (( expression))

            | NEG expression ((NEG(  expression))) 
            | INVERSE LPAREN expression RPAREN ((INVERSE( expression))) 
            | RATFUN LPAREN expression RPAREN ((RATFUN( expression))) 
            | MAKERAT LPAREN expression COMMA expression RPAREN  ((make_rat(expression1,expression2)))
            | FROMDECIMAL LPAREN expression RPAREN ((fromDecimal(expression)))
            | TT ((TT ))
            | FF ((FF ))
            
            | POSNUMERAL ((Posnumeral(POSNUMERAL) ))

            | RATIONAL (( Rational_numeral(RATIONAL) ))

            | IDENTIFIER (( Identifier(IDENTIFIER) ))

            | NOT expression ( (NOT( expression)) )
