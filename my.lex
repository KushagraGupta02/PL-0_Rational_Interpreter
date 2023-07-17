structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)

%%
%header (functor MyLexFun(structure Tokens: My_TOKENS));
%s PL0_RATIONAL COMMENT;
alpha=[A-Za-z];
ascii=[\x20-\7F];
digit=[0-9];
ws = [\ \t];
%%
<INITIAL>{ws}* => (YYBEGIN PL0_RATIONAL; continue());
<PL0_RATIONAL> {ws}+ => (continue());
<PL0_RATIONAL> \n       => ( pos:= (!pos) + 1; lex()); 
<COMMENT> \n       => ( pos:= (!pos) + 1; lex()); 
<PL0_RATIONAL>"(*" => (YYBEGIN COMMENT; continue());
<PL0_RATIONAL>"tt" => ( Tokens.TT(!pos , !pos ) ) ;
<PL0_RATIONAL>"ff" => (Tokens.FF(!pos , !pos )) ;  
<PL0_RATIONAL>"("      => (Tokens.LPAREN(!pos , !pos ));
<PL0_RATIONAL>")"      => (Tokens.RPAREN(!pos , !pos )) ; 
<PL0_RATIONAL>"}"      => (Tokens.RBRACE(!pos , !pos )) ; 
<PL0_RATIONAL>"{"      => (Tokens.LBRACE(!pos , !pos )) ; 
<PL0_RATIONAL>"!"      => (Tokens.NOT(!pos , !pos )) ; 
<PL0_RATIONAL>"&&"       => ( Tokens.AND(!pos , !pos )) ; 
<PL0_RATIONAL>"||"       => (Tokens.OR(!pos , !pos )) ;
<PL0_RATIONAL>";"       => (Tokens.SEMICOLON(!pos , !pos )) ;
<PL0_RATIONAL>","       => (Tokens.COMMA(!pos , !pos )) ;
<PL0_RATIONAL>":="       => (Tokens.ASSIGN(!pos , !pos )) ;
<PL0_RATIONAL>"+"       => (Tokens.PLUS(!pos , !pos )) ;
<PL0_RATIONAL>"-"       => (Tokens.MINUS(!pos , !pos )) ;
<PL0_RATIONAL>"*"       => (Tokens.TIMES(!pos , !pos )) ;
<PL0_RATIONAL>"/"       => (Tokens.DIV(!pos , !pos )) ;
<PL0_RATIONAL>"%"       => (Tokens.MOD(!pos , !pos )) ;
<PL0_RATIONAL>".+."       => (Tokens.RPLUS(!pos , !pos )) ;
<PL0_RATIONAL>".-."       => (Tokens.RMINUS(!pos , !pos )) ;
<PL0_RATIONAL>".*."       => (Tokens.RTIMES(!pos , !pos )) ;
<PL0_RATIONAL>"./."       => (Tokens.RDIV(!pos , !pos )) ;
<PL0_RATIONAL>"read"       => (Tokens.READ(!pos , !pos )) ;
<PL0_RATIONAL>"print"       => (Tokens.PRINT(!pos , !pos )) ;
<PL0_RATIONAL>"if"       => (Tokens.IF(!pos , !pos )) ;
<PL0_RATIONAL>"then"       => (Tokens.THEN(!pos , !pos )) ;
<PL0_RATIONAL>"else" => (Tokens.ELSE(!pos , !pos )) ; 
<PL0_RATIONAL>"fi"       => (Tokens.FI(!pos , !pos )) ;
<PL0_RATIONAL>"while"       => (Tokens.WHILE(!pos , !pos )) ;
<PL0_RATIONAL>"od"       => (Tokens.OD(!pos , !pos )) ;
<PL0_RATIONAL>"do"       => (Tokens.DO(!pos , !pos )) ;
<PL0_RATIONAL>"rational"       => (Tokens.RAT(!pos , !pos )) ;
<PL0_RATIONAL>"integer"       => (Tokens.INT(!pos , !pos )) ;
<PL0_RATIONAL>"boolean"       => (Tokens.BOOL(!pos , !pos )) ;
<PL0_RATIONAL>"procedure"       => (Tokens.PROCEDURE(!pos , !pos )) ;
<PL0_RATIONAL>"inverse"         => (Tokens.INVERSE(!pos , !pos )) ;
<PL0_RATIONAL>"rat"         => (Tokens.RATFUN(!pos , !pos )) ;
<PL0_RATIONAL>"call"       => (Tokens.CALL(!pos , !pos )) ;
<PL0_RATIONAL>"make_rat"  => (Tokens.MAKERAT(!pos , !pos )) ;
<PL0_RATIONAL>"fromDecimal" => (Tokens.FROMDECIMAL(!pos , !pos )) ;
<PL0_RATIONAL>"<="       => (Tokens.LEQ(!pos , !pos )) ;
<PL0_RATIONAL>">="       => (Tokens.GEQ(!pos , !pos )) ;
<PL0_RATIONAL>">"       => (Tokens.GT(!pos , !pos )) ;
<PL0_RATIONAL>"="       => (Tokens.EQ(!pos , !pos )) ;
<PL0_RATIONAL>"<>"       => (Tokens.NEQ(!pos , !pos )) ;
<PL0_RATIONAL>"<"       => (Tokens.LT(!pos , !pos )) ;
<PL0_RATIONAL>"~" => (Tokens.NEG(!pos , !pos)) ; 
<PL0_RATIONAL>{alpha}({alpha} | {digit})* => ( Tokens.IDENTIFIER( yytext ,!pos , !pos )) ; 
<PL0_RATIONAL>({digit}){digit}* => ( Tokens.POSNUMERAL( yytext , !pos , !pos ) ) ; 
<PL0_RATIONAL>[~]?{digit}*"."{digit}*"("{digit}+")" => (Tokens.RATIONAL ((yytext), !pos, !pos));
<COMMENT>. => (continue());
<COMMENT>"*)" => (YYBEGIN PL0_RATIONAL;continue());