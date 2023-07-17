structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)

%%
%header (functor MyLexFun(structure Tokens: My_TOKENS));
alpha=[A-Za-z];
ascii=[\x20-\7F];
digit=[0-9];
ws = [\ \t];
%%
\n       => ( pos:= (!pos) + 1; lex()); 
{ws}+    => (lex());
"tt" => ( Tokens.TT(!pos , !pos ) ) ;
"ff" => (Tokens.FF(!pos , !pos )) ;  
"("      => (Tokens.LPAREN(!pos , !pos ));
")"      => (Tokens.RPAREN(!pos , !pos )) ; 
"}"      => (Tokens.RBRACE(!pos , !pos )) ; 
"{"      => (Tokens.LBRACE(!pos , !pos )) ; 
"!"      => (Tokens.NOT(!pos , !pos )) ; 
"&&"       => ( Tokens.AND(!pos , !pos )) ; 
"||"       => (Tokens.OR(!pos , !pos )) ;
";"       => (Tokens.SEMICOLON(!pos , !pos )) ;
","       => (Tokens.COMMA(!pos , !pos )) ;
":="       => (Tokens.ASSIGN(!pos , !pos )) ;
"+"       => (Tokens.PLUS(!pos , !pos )) ;
"-"       => (Tokens.MINUS(!pos , !pos )) ;
"*"       => (Tokens.TIMES(!pos , !pos )) ;
"/"       => (Tokens.DIV(!pos , !pos )) ;
"%"       => (Tokens.MOD(!pos , !pos )) ;
".+."       => (Tokens.RPLUS(!pos , !pos )) ;
".-."       => (Tokens.RMINUS(!pos , !pos )) ;
".*."       => (Tokens.RTIMES(!pos , !pos )) ;
"./."       => (Tokens.RDIV(!pos , !pos )) ;
"read"       => (Tokens.READ(!pos , !pos )) ;
"print"       => (Tokens.PRINT(!pos , !pos )) ;
"if"       => (Tokens.IF(!pos , !pos )) ;
"then"       => (Tokens.THEN(!pos , !pos )) ;
"else" => (Tokens.ELSE(!pos , !pos )) ; 
"fi"       => (Tokens.FI(!pos , !pos )) ;
"while"       => (Tokens.WHILE(!pos , !pos )) ;
"od"       => (Tokens.OD(!pos , !pos )) ;
"do"       => (Tokens.DO(!pos , !pos )) ;
"rational"       => (Tokens.RAT(!pos , !pos )) ;
"integer"       => (Tokens.INT(!pos , !pos )) ;
"boolean"       => (Tokens.BOOL(!pos , !pos )) ;
"procedure"       => (Tokens.PROCEDURE(!pos , !pos )) ;
"inverse"         => (Tokens.INVERSE(!pos , !pos )) ;
"rat"         => (Tokens.RATFUN(!pos , !pos )) ;
"call"       => (Tokens.CALL(!pos , !pos )) ;
"make_rat"  => (Tokens.MAKERAT(!pos , !pos )) ;
"fromDecimal" => (Tokens.FROMDECIMAL(!pos , !pos )) ;
"<="       => (Tokens.LEQ(!pos , !pos )) ;
">="       => (Tokens.GEQ(!pos , !pos )) ;
">"       => (Tokens.GT(!pos , !pos )) ;
"="       => (Tokens.EQ(!pos , !pos )) ;
"<>"       => (Tokens.NEQ(!pos , !pos )) ;
"<"       => (Tokens.LT(!pos , !pos )) ;
"~" => (Tokens.NEG(!pos , !pos)) ; 
{alpha}({alpha} | {digit})* => ( Tokens.IDENTIFIER( yytext ,!pos , !pos )) ; 
({digit}){digit}* => ( Tokens.POSNUMERAL( yytext , !pos , !pos ) ) ; 
[~]?{digit}*"."{digit}*"("{digit}+")" => (Tokens.RATIONAL ((yytext), !pos, !pos));
"(*"(.|\n)*"*)" => (lex());