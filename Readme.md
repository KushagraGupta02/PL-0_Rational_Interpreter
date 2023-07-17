## COL226 Assignment 4: PL0 + Rationals (2021CS50592)
### **1) Grammar**
#### b) Grammar for **input file**

It may be represented by the following EBNF:

    Program ::= Block .
    Block ::= DeclarationSeq CommandSeq .
    DeclarationSeq ::= [VarDecls] [ProcDecls] .
    VarDecls ::= [RatVarDecls] [IntVarDecls] [BoolVarDecls] .
    RatVarDecls ::= rational Ident {, Ident}; .
    IntVarDecls ::= integer Ident {, Ident}; .
    BoolVarDecls ::= boolean Ident {, Ident}; .
    ProcDecls ::= [ProcDef {;ProcDecls};] .
    ProcDef ::= procedure Ident Block .
    CommandSeq ::= {{Command;}} .
    Command ::= AssignmentCmd | CallCmd | ReadCmd | P rintCmd |
    ConditionalCmd | WhileCmd .
    AssignmentCmd ::= Ident := Expression .
    CallCmd ::= call Ident .
    ReadCmd ::= read( Ident ) .
    PrintCmd ::= print( Expression ) .
    Expression ::= RatExpression | IntExpression | BoolExpression .
    ConditionalCmd ::= if BoolExpression then CommandSeq else CommandSeq fi .
    WhileCmd ::= while BoolExpression do CommandSeq od .


Note: The grammar implemented in RatCalc.lex (as follows) is **equivalent** to the above, since we have defined the precedence rules as well:

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

### **2)Instructions for Use**
This is our interactive rational calculator built using ML-Yacc and ML-Lex.
The calculator is defined by the files

    Rational.sml    (* BigInt and Rational library*)
    datatypes.sml  (* Lot of datatypes and AST evaluation function*)
    my.lex         (* Lexer rules. *)
    my.yacc:MLYacc (* Parser rules. *)
    glue.sml       (* Build the parser *)
    interpret.sml (* Lex, parse *)

To compile type

    sudo sml;
    - CM.make "my.cm"; 

in the directory. 

CM will invoke ml-lex and ml-yacc to process the lexer specification and the grammar specification which will compile these SML source files

    Library
        structure My
    is
    Rational.sml    (* BigInt and Rational library*)
    datatypes.sml  (* Lot of datatypes *)
    my.lex         (* Lexer rules. *)
    my.yacc:MLYacc (* Parser rules. *)
    glue.sml       (* Build the parser *)
    interpret.sml (* Lex, parse *)
    $/basis.cm (* SML/NJ’s Basis Library. *)
    $/ml-yacc-lib.cm 
    $smlnj/compiler/compiler.cm 
    $/smlnj-lib.cm 

The end result of loading these files is a structure My containing a top-level driver function named parse. which can be invoked by applying Make_AST and interpret to the unit value

    My.interpret ( "example-rec.rat", "outputFile"); : (string,string) -> (string * ?.DataTypes.Type * string * int) list ref *
    (string * ?.DataTypes.CMD list * int list) list ref * int list ref
    -  My.interpret ( "example-rec.rat", "outputFile"); 
    My.Make_AST () : string -> : ?.MyParser.result *
    (?.MyParser.svalue,int) ?.LrParser.Token.token ?.LrParser.stream

The interpreter reads a sequence of expressions from the file provided and saves the printed value in the other file after reading the expression.

### **3)Design Decisions undertaken**
- **COMMENTS** Comments are made using states (similar to Pascal example in documentation) in the file my.lex so we can have comments at different portions of the text, but not nested comments. If we use temp.lex, and at different portions it will comment the whole text and nested also. (pattern matching - first and last opening and closing)
- Procedures must have their unique identifiers
- Our design involves that first the Procedures are declared traversing recursively in each Procedure List and their scopes are assigned numbers. They are stored in the Procedures list along with their commands list. So, the procedures need not be declared earlier before their call (or in the same scope).
- SymbolTable stores the variables declared with their scope numbers, and the updates are done by traversing recursively over the STATIC links associated with every procedure/scope to their earliest declartation of the variable. So, it might be possible that due to nested function calls of the same function, the new value (changed) is reflected instead of old one to implement STATIC SCOPING: _This was chosen over Dyanmic Scoping for Static Scoping_
- The errors are of 3 types - UnDeclaredVariableException; VariableRedeclarationException of string and finally TypeMismatch - for Bool, Int and Rational Operators for TypeMismatch so errors reported from the functions used to compute the expression : Like - rat_error, fail not an integer

**USING INT INSTEAD OF BIGINT** - 

For efficiency, whenever integers and their operations are well within the (valOf Int.maxInt) and (valOf Int.minInt) limits you may use the SML-NJ system defined Int structure. However, there can be no overflows or underflows in integer operations, Then we can modify the grammar as follows : 

    START : PRINT EXP (print (EXP);
                     print "\n";
                     SOME EXP)
      |  EXP (SOME EXP)
      | (NONE)

    EXP : REXP            (REXP)
      | IEXP            (IEXP)
      | BEXP            (BEXP)
    IEXP : NUM             (NUM)
        | IEXP PLUS IEXP    (Int.toString(valOf(Int.fromString(IEXP1))+valOf(Int.fromString(IEXP2))))
        | IEXP TIMES IEXP   (Int.toString(valOf(Int.fromString(IEXP1))*valOf(Int.fromString(IEXP2))))
        | IEXP DIV IEXP     (Int.toString(valOf(Int.fromString(IEXP1)) div valOf(Int.fromString(IEXP2))))
        | IEXP SUB IEXP     (Int.toString(valOf(Int.fromString(IEXP1))-valOf(Int.fromString(IEXP2))))
        | IEXP MOD IEXP     (Int.toString(valOf(Int.fromString(IEXP1)) mod valOf(Int.fromString(IEXP2))))
        | NEG EXP          ("~"^(EXP1))
        | LPAREN IEXP RPAREN ((IEXP1))
    
    Taking into account under and overflows, depending upon our computations
    
### **4)SAMPLE CASES**
1. **inputFile.rat"**

        integer factorialArg, factorialResult;
        procedure factorialRec
        integer i, acc;

        procedure factorialIter
            {
                if (i <= factorialArg) then
                {
                    acc := acc*i;
                    i := i+1;
                    call factorialIter;
                } else {} fi;
            };
        {
            i := 1;
            acc := 1;
            call factorialIter;
            factorialResult := acc;
        };

        {
            factorialArg := 10;
            call factorialRec;
            print(factorialResult);
        }
    ```
    CM.make "my.cm";
    My.interpret("inputFile.rat","outputFile")
    ```
        Enter the value of factorialArg
        10
    **File output contents**

    **"outputFile"**

        3628800
    **Terminal output** -- Printing the contents and returning the SymbolTable, ProcedureArray, CurrentLink 

        3628800
        val it =
        (ref
            [("i",Int,"11",1),("factorialResult",Int,"3628800",0),
            ("acc",Int,"3628800",1),("factorialArg",Int,"10",0)],
        ref
            [("factorialIter",
            [ITE
                (LEQ (Identifier "i",Identifier "factorialArg"),
                [SET ("acc",TIMES (Identifier "acc",Identifier "i")),
                    SET ("i",PLUS (Identifier "i",Posnumeral "1")),
                    Call "factorialIter"],[])],[2,1,0]),
            ("factorialRec",
            [SET ("i",Posnumeral "1"),SET ("acc",Posnumeral "1"),
                Call "factorialIter",SET ("factorialResult",Identifier "acc")],[1,0])],
        ref [0])
        : (string * ?.DataTypes.Type * string * int) list ref *
            (string * ?.DataTypes.CMD list * int list) list ref * int list ref

2. Exception Handling -- UnDeclaredVariableException; TypeMisMatchException ; VariableRedeclarationException of string ; 

    ```
    {
        x:= 20;
        print(x+tt);
        print(x);
        cc:= make_rat(6,7);
        d:= make_rat(6,7);
    }
    ```

    Output 
    ```My.interpret ( "e", "outputFile");
    uncaught exception UnDeclaredVariableException
    raised at: datatypes.sml:60.28-60.55
    ```
3. Exception Handling -- Using typechecking - for Bool, Int and Rational Operators for TypeMismatch so errors reported from the functions used to compute the expression -------------------- Like - rat_error, fail not an integer
    ```
    rational cc,d;
    integer x;
    {
        x:= 20;
        print(x+tt);
        print(x);
        cc:= make_rat(6,7);
        d:= make_rat(6,7);
    }
    ```

    Output 
    ```My.interpret ( "e", "outputFile");
    uncaught exception Fail [Fail: not an integer]
    raised at: Rational.sml:50.21-50.42
    ```
### **5)Acknowledgements**
The Pascal and Calculator examples documented on how to use ML-LEX and ML-YACC in the ml-yacc package were used. 
They are obtained from the official ml-yacc documentation (path as follows) and the files calc.grm, calc.lex, calc.sml provided inspiration for our files my.yacc, my.lex and interpret.sml.
1. <a href = "http://rogerprice.org/ug/ug.pdf" > User's guide to ML-Lex and ML-Yacc </a>
2. <a href="https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html" target="_blank">ML-Yacc Princeton Manual</a>

Code used in ``` interpret.sml``` , ```glue.sml```, ```my.cm```  was inspired from 1.

    /usr/share/doc/ml-yacc/examples/calc$   and /usr/share/doc/ml-yacc/examples/Pascal$ 
    ls
    README  calc.grm  calc.lex  calc.sml  sources.cm

The regular expression for comments was obtained by the Pascal example in the documentation.
I would like to acknowledge the ml-yacc developers and contributors for their valuable examples.